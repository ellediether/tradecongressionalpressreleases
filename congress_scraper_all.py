import asyncio
import pandas as pd
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
from urllib.parse import urljoin

# Function to scrape congress member's website
async def scrape_congress_member(name, base_url, page):
    press_releases = []
    current_url = base_url
    page_number = 1
    max_pages = 50  # Limit to 50 pages to avoid infinite loops
    visited_urls = set()

    while page_number <= max_pages:
        print(f"\n🔍 Scraping {name} - Page {page_number}: {current_url}")
        try:
            await page.goto(current_url, timeout=30000)
            await page.wait_for_timeout(2000)  # Allow time for dynamic content to load

            # Auto-scroll the page to load more content
            await auto_scroll(page)

            html = await page.content()
            soup = BeautifulSoup(html, "html.parser")

            # Collect press release links based on the page content
            links = []
            press_release_links = soup.find_all('a', href=True)
            for link in press_release_links:
                href = link['href']
                if 'press-release' in href or 'news' in href:  # Check if the link is a press release
                    full_link = urljoin(current_url, href)  # Make the URL absolute
                    links.append({
                        "Member": name,
                        "Link": full_link
                    })

            if links:
                press_releases.extend(links)
                print(f"✅ Found {len(links)} press releases for {name}")
            else:
                print(f"⛘ No press releases found for {name} on page {page_number}")
                break

            # Check if we are stuck on the original page (avoid getting stuck in a loop)
            if current_url in visited_urls:
                print(f"❌ Already visited {current_url}, moving to next congressman.")
                break
            visited_urls.add(current_url)

            # Handle pagination if next button exists
            next_button = None
            next_buttons = await page.query_selector_all('a[href*="next"], a[aria-label="Next"], button[aria-label="Next"], a[rel="next"], .next, .pagination')  # Check for common next buttons
            if next_buttons:
                print(f"✅ Found 'Next' button.")
                next_button = next_buttons[0]
                next_page_url = await next_button.get_attribute('href')
                current_url = urljoin(current_url, next_page_url)  # Update the current URL for the next page
                await next_button.click()  # Click on the "Next" button
                await page.wait_for_timeout(3000)  # Allow time for the next page to load
                page_number += 1
            else:
                print(f"❌ No valid 'Next' button found for {name}")
                break

        except Exception as e:
            print(f"❌ Error scraping {name}: {e}")
            break

    return press_releases

# Auto scroll to ensure more content is loaded
async def auto_scroll(page, scroll_pause_time=500, max_scrolls=10):
    for _ in range(max_scrolls):
        await page.mouse.wheel(0, 1500)  # Scroll down slowly
        await page.wait_for_timeout(scroll_pause_time)  # Wait for content to load

async def main():
    congress_df = pd.read_csv("118thCongressMembers.csv")  # Ensure correct path to your CSV

    all_data = []

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False)
        page = await browser.new_page()

        # Loop through each congress member and scrape their site
        for idx, row in congress_df.iterrows():
            name = f"{row['first_name']} {row['last_name']}"
            website = str(row["website"]).strip()
            if not website or not website.startswith("http"):
                print(f"⚠️ Skipping {name}: Invalid website")
                continue

            # Scrape the press releases from the congress member's press release page
            press_links = await scrape_congress_member(name, website, page)
            all_data.extend(press_links)

        await browser.close()

    # Save the results to a DataFrame and output to CSV
    df_out = pd.DataFrame(all_data).drop_duplicates()
    df_out.to_csv("all_congress_press_releases.csv", index=False)
    print(f"\n✅ Done! Saved {len(df_out)} press releases.")
    print(df_out.head())


if __name__ == "__main__":
    asyncio.run(main())
