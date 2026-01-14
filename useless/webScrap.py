import requests
from bs4 import BeautifulSoup
import csv
from datetime import datetime

# List of URLs to scrape
urls = [
    "https://www.toppreise.ch/comparison-prix/Appareils-photo-numeriques/FUJIFILM-X100VI-Black-4178983-p758226",
    "https://www.toppreise.ch/comparison-prix/Appareils-photo-numeriques/FUJIFILM-X100VI-Silver-4178982-p758227?selsort=rd",
    "https://www.toppreise.ch/comparison-prix/Projecteurs/SONY-VPL-XW5000ES-Black-p693732?selsort=rd",
    "https://www.toppreise.ch/comparison-prix/Appareils-photo-numeriques/FUJIFILM-X100V-Silver-1012685-p598010?selsort=rd",
    "https://www.toppreise.ch/comparison-prix/Convertisseurs/FUJIFILM-AR-X100-Silver-p317684?selsort=rd"
]

# Fake browser headers to bypass 403 errors
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36",
    "Accept-Language": "en-US,en;q=0.9",
    "Referer": "https://www.google.com/"
}

# Output CSV file
csv_filename = "products_data.csv"

# Open CSV file for writing
with open(csv_filename, mode='w', newline='', encoding='utf-8') as file:
    
    writer = csv.writer(file)
    # Write the headers
    writer.writerow(['Date and Time', 'Product Brand', 'Product Name', 'Price (CHF)', 'Availability'])

    for url in urls:
        try:
            # Send GET request
            response = requests.get(url, headers=headers)
            if response.status_code != 200:
                print(f"Failed to retrieve {url} (Status Code: {response.status_code})")
                continue
            
            # Parse HTML
            soup = BeautifulSoup(response.text, 'html.parser')

            # Extract product brand
            product_section = soup.find("h1", class_='product-name m-0')
            product_brand_tag = product_section.find('span', class_='manu')
            product_brand = product_brand_tag.get_text(strip=True) if product_brand_tag else 'Product Brand Not Found'
            
            product_name_tag = product_section.find('span', class_=['title','title break'])
            product_name = product_name_tag.get_text(strip=True) if product_name_tag else 'Product Name Not Found'
 
            # Extract price
            price_section = soup.find("div", id='Plugin_HashedPrice_578526')
            price_tag = price_section.find('div', class_='Plugin_Price')
            price = price_tag.get_text(strip=True) if price_tag else 'Price not found'
            

            # Extract first occurrence of <i class="TPIcons-avail_1"></i> inside <div class="f_standardList">
            availability_section = soup.find('div', class_='f_standardList')
            if availability_section:
                availability_icon = availability_section.find('i', class_='TPIcons-avail_1')
                availability = 'Available=1' if availability_icon else 'Not Available'
            else:
                availability = 'Availability Section Not Found'

            # Get the current timestamp
            now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')

            # Write to CSV
            writer.writerow([now, product_brand, product_name, price, availability])

            print(f"Scraped: {product_brand} - {product_name} - Price: {price}, Availability: {availability}")

        except Exception as e:
            print(f"Error processing {url}: {e}")

print(f"\nScraping completed. Data saved to {csv_filename}")
