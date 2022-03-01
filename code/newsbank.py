from bs4 import BeautifulSoup
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import csv
from math import ceil
import re

SHORT_TEST = True  # testing flag
GET_NUMBER = True
NUMBER = 50

KEYWORD = "demonetization"
CLOSENESS = 10

bigrams = ["might again", "could happen", "happen again", "do again", "it again", "never happen", "never again",
           "government may", "RBI may", "likely to", "unlikely to", "likely never", "likely will", "possible that",
           "once again"]  # consider splitting into positive/negative sentiments


def findProximity(text, bigram, keyword, closeness):  # modify to search through library of bigrams
    sections = []
    words = text.split(" ")
    for i in range(len(words) - 1):
        if words[i] == bigram[0] and words[i + 1] == bigram[1]:
            subsection = words[max(0, i - closeness):min(len(words), i + 1 + closeness)]
            if keyword in subsection:
                print("found")
                sections.append(" ".join(subsection))
    return sections


def findProximityRegex(text):
    matches = []

    str_rep = "(" + "|".join(bigrams) + ")"

    expression = "(\\w+\\W+){," + str(CLOSENESS) + "}" + str_rep + "(\\W+\\w+){," + str(CLOSENESS) + "}"  # TODO: fix regex to find overlapping matches

    regex_substring = re.compile(expression)

    for match in regex_substring.finditer(text):
        substring = text[match.start():match.end()]
        if re.search("demoneti[sz]ation", substring):
            matches.append(text[match.start():match.end()])

    # if not matches:
    #     print("no matches found")
    # else:
    #     print("matches found")
    return matches



# sample = "A study by the National Investigation Agency and the Indian government may Statistical government Institute, in 2016, estimated that fake Indian happen again currency notes in circulation have a face value of Rs. 400 crore. This is an incidence of fake currency of 0.022%. The scale of counterfeiting of the Indian rupee is not out of line with what is seen in other countries, and the procedures adopted worldwide to address this include investigative actions against counterfeiters, phased replacement of old series of notes with new notes that have better security features, etc. Demonetisation is generally not seen as a tool for dealing with counterfeiting. We must also not forget that the counterfeiters will now get to work on the new 500/2000 rupee notes, while India will likely never do a demonetisation again."
#
# print(findProximityRegex(sample))

base_url = "https://infoweb-newsbank-com.dartmouth.idm.oclc.org"
temp_base_url = "https://infoweb.newsbank.com"
# newsbank_url_best_match = "https://infoweb-newsbank-com.dartmouth.idm.oclc.org/apps/news/results?p=AWNB&t=continent%3AAsia%21Asia&sort=_rank_%3AD&maxresults=20&f=advanced&val-base-0=demonetization&fld-base-0=alltext&bln-base-1=and&val-base-1=11/08/2016-2/08/2017&fld-base-1=YMD_date"

# page 0
newsbank_url = "https://infoweb-newsbank-com.dartmouth.idm.oclc.org/apps/news/results?page=0&p=AWNB&t=continent%3AAsia%21Asia&sort=_rank_%3AD&maxresults=20&f=advanced&val-base-0=demonetization&fld-base-0=alltext&bln-base-1=and&val-base-1=11/08/2016-2/08/2017&fld-base-1=YMD_date"
temp_newsbank_url = "https://infoweb.newsbank.com/apps/news/results?page=0&p=AWNB&t=continent%3AAsia%21Asia&sort=_rank_%3AD&maxresults=20&f=advanced&val-base-0=demonetization&fld-base-0=alltext&bln-base-1=and&val-base-1=11/08/2016-02/08/2017&fld-base-1=YMD_date"

chrome_options = Options()
chrome_options.add_argument("--headless")
browser = webdriver.Chrome("/Users/meganren/Downloads/chromedriver", options=chrome_options)
print("fetching initial page...")
browser.get(temp_newsbank_url)
selenium_soup = BeautifulSoup(browser.page_source, "html.parser")

# print(selenium_soup.prettify())

fields = ["Title", "Source", "Date", "URL", "Sentences"]
articles = []
csv_file = "newsbank.csv"

# num_results = int(selenium_soup.find("div", class_="search-hits__meta--total_hits").text.split()[0])
print("calculating search results page length...")
num_results = int(selenium_soup.find("div", class_="search-hits__meta--total_hits").text.split()[0].replace(",", ""))
num_pages = ceil(num_results/20)

find = temp_newsbank_url.find("page")

try:
    with open(csv_file, "w") as f:
        dict_writer = csv.DictWriter(f, fieldnames=fields)
        dict_writer.writeheader()
except IOError:
    print("error writing to file")


pages_searched = 0
num_found = 0
for i in range(num_pages):  # overarching for loop
    if SHORT_TEST and i > 0:
        break
    if GET_NUMBER and num_found > NUMBER:
        break
    print("found: " + str(num_found))
    # print("getting", j, "th page...")
    browser.get(temp_newsbank_url[:find + 5] + str(j) + temp_newsbank_url[find + 5 + min(len(str(j - 1)),
                                                                                         1):])  # insert appropriate page number
    results_soup = BeautifulSoup(browser.page_source, "html.parser")

    links = []
    result_headers = results_soup.find_all("h3", class_="search-hits__hit__title")
    for article in result_headers:
        links.append(temp_base_url + article.a["href"])

    for link in links:
        print("handling", link)
        browser.get(link)
        article_soup = BeautifulSoup(browser.page_source, "html.parser")
        main_content = article_soup.find("div",
                                         class_="document-view__content__inner")  # not div with id "main-content", be careful

        if not main_content:
            print("whoops something went wrong with", link)
            print(article_soup.prettify())
            continue

        text = main_content.find("div", class_="document-view__body").text

        matches = findProximityRegex(text)
        if matches:
            # print("inside")
            meta = main_content.find(class_="document-view__meta__item")
            date = datetime.strptime(meta.find(class_="display-date").text, "%B %d, %Y")

            sub_dict = {}
            sub_dict["Title"] = main_content.h1.text
            sub_dict["Source"] = meta.find(class_="source").text
            sub_dict["Date"] = datetime.strptime(meta.find(class_="display-date").text, "%B %d, %Y")
            url = article_soup.find("div", class_="actions-bar__urltext")
            sub_dict["URL"] = url.text if url else link
            sub_dict["Sentences"] = "|".join(matches)

            try:
                with open(csv_file, "a") as f:
                    dict_writer = csv.DictWriter(f, fieldnames=fields)
                    dict_writer.writerow(sub_dict)
            except IOError:
                print("error writing to file")

            articles.append(sub_dict)
            num_found += 1
    pages_searched += 1
    print("NEW PAGE")

print("DONE")
print(articles)

print("pages searched:", pages_searched)
