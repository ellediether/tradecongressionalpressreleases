import asyncio
import pandas as pd
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
import re
from urllib.parse import urljoin
from datetime import datetime

# Function to extract a date from the text using common date formats
def extract_us_date_from_text(text):
    pattern = r"\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\s+\d{1,2},\s+\d{4}\b|\b\d{1,2}/\d{1,2}/\d{4}\b"  # Matches dates like "January 1, 2022" and "01/01/2022"
    match = re.search(pattern, text)
    return match.group(0) if match else None

# Function to normalize different date formats
def normalize_date(date_str):
    try:
        # Try parsing known date formats
        return datetime.strptime(date_str, '%B %d, %Y').strftime('%Y-%m-%d')  # Example: "January 1, 2022"
    except ValueError:
        try:
            return datetime.strptime(date_str, '%m/%d/%Y').strftime('%Y-%m-%d')  # Example: "01/01/2022"
        except ValueError:
            return None  # If no valid format found

# Function to extract the largest paragraph
def extract_largest_paragraph(text):
    paragraphs = re.split(r'\n{2,}', text)
    longest = max(paragraphs, key=lambda p: len(p.strip()), default="")
    return longest.strip()

# -------------------------------
# MAIN SCRAPER FUNCTION
# -------------------------------

async def scrape_press_release_details(input_csv="press_releases_body_full.csv", output_csv="press_releases_details_full.csv"):
    df = pd.read_csv(input_csv)  # Full sample CSV file
    df["Date"] = ""
    df["Body"] = ""
    df["Title"] = ""

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False, slow_mo=100)  # Slow it down
        context = await browser.new_context()
        page = await context.new_page()

        for idx, row in df.iterrows():
            url = row["Link"]
            print(f"\n🔗 Visiting [{idx+1}] {url}")

            try:
                await asyncio.wait_for(page.goto(url, timeout=60000), timeout=120)
                await page.wait_for_timeout(2000)  # Allow some time for the page to load

                # ---------------------
                # Scroll slowly at the top to load content
                # ---------------------
                for _ in range(10):  # 10 slow scrolls at the top
                    await page.mouse.wheel(0, 1000)  # Slow scroll down
                    await page.wait_for_timeout(1000)  # Wait for content to load

                # After scrolling slowly, go faster
                for _ in range(5):  # 5 faster scrolls
                    await page.mouse.wheel(0, 1500)  # Faster scroll down
                    await page.wait_for_timeout(500)  # Shorter wait after fast scroll

                # ---------------------
                # 📅 DATE EXTRACTION (looking for different formats)
                # ---------------------
                date = None
                try:
                    # Attempt to extract the date from common tags (e.g., time, header, etc.)
                    date = await page.locator("time").first.text_content()  # Check <time> tag for the date
                    if date:
                        date = normalize_date(date.strip())  # Normalize the date format
                except:
                    pass  # No <time> tag

                if not date:
                    # If no date in <time> tag, search for other date patterns around title or content
                    full_text = await page.text_content("body")
                    date = extract_us_date_from_text(full_text)
                    date = normalize_date(date) if date else None

                # ---------------------
                # 📜 BODY EXTRACTION
                # ---------------------
                body = extract_largest_paragraph(full_text)

                # ---------------------
                # 📑 TITLE EXTRACTION
                # ---------------------
                title = await page.title()

                # Save results in the DataFrame
                df.at[idx, "Title"] = title if title else "❓ Not found"
                df.at[idx, "Date"] = date if date else "❓ Not found"
                df.at[idx, "Body"] = body if body else "⚠️ No readable body"

            except Exception as e:
                print(f"❌ Error: {e}")
                df.at[idx, "Date"] = "❌"
                df.at[idx, "Body"] = f"❌ {e}"

        await browser.close()

    df.to_csv(output_csv, index=False)
    print(f"\n✅ Done! Output saved to '{output_csv}'")
    print(df[["Member", "Title", "Date"]].head())

# -------------------------------
# RUN SCRIPT
# -------------------------------

asyncio.run(scrape_press_release_details())
