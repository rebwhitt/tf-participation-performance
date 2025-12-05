from bs4 import BeautifulSoup
import requests
import csv
import os
import re

def scrape_list(url, filename):
    # get and load the webpage as the variable "soup" from the above url
    response = requests.get(url, verify=False, headers={'User-Agent':'rebeccabot'})
    print("scraping",url)
    soup = BeautifulSoup(response.content, 'html.parser')
    #print(soup)
    links = []
    print(url)
    for a in soup.find_all("a"):
        #print(a.get('href'))
        if(not a.get('href')==None):
            if re.match(f'{url}/[0-9]+/', a.get('href')):
                links.append(a.get('href').replace("formatted","raw"))
    
    year = re.findall("-[12][90][0-9][0-9]/", url)[0].replace("-","").replace("/","")
    rows = []
    for link in links:
        response = requests.get(link, verify=False, headers={'User-Agent':'rebeccabot'})
        print("scraping",link)
        minisoup = BeautifulSoup(response.content, 'html.parser')
        if (len(minisoup.find_all("pre"))>0):
            rows.append({"Year":year, "Text":minisoup.find_all("pre")[0].text})
        else:
            rows.append({"Year":year, "Text":minisoup.find("article").text})

        # then open the file based on the filename passed in
    fields = ["Year","Text"]
    with open(filename, 'w', newline='', encoding="utf-8") as file:
        writer = csv.DictWriter(file, fieldnames=fields)
        writer.writeheader()

        # and write each row of data into the file
        for item in rows:
            writer.writerow(item)

    print(len(rows),"rows of data scraped and saved to",filename)

    #print(links)
    #print(soup.find_all("pre")[0].text)

if __name__ == "__main__":
    years = []
    with open('ca_links_year.csv', 'r') as csvfile:
        reader=csv.DictReader(csvfile)
        for row in reader:
            years.append(f"{row['url']}/results")

    for i in range(19,len(years)):
    #for i in range(18,19):
        url = years[i]
        year = re.findall("-[12][90][0-9][0-9]/", url)[0].replace("-","").replace("/","")
        filename = os.path.dirname(__file__)+f"/Raw/text{year}.csv"
        scrape_list(url, filename)
