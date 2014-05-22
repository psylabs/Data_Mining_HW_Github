__author__ = 'mo'
import re
import mechanize
from bs4 import BeautifulSoup
import csv
import time
import io

br = mechanize.Browser()
data=[]
urls=[]
br.open("http://beta.congress.gov/search?q=%7B%22congress%22%3A%5B%22113%22%2C112%2C111%5D%2C%22search%22%3A%22affordable+care+act%22%2C%22source%22%3A%22congrecord%22%7D&pageSize=500")
html = br.response().read()
soup = BeautifulSoup(html)

baseurl = "http://beta.congress.gov"
i=0
links = soup.find_all('a')
for link in links:
    if link.parent.name == 'h2':
        i+=1
        newurl=baseurl+link.get('href')
        print(newurl)
        urls.append(newurl)
        req = br.open(newurl)
        html2 = br.response().read()
        soup2 = BeautifulSoup(html2)
        content=soup2.find("pre")
        # content=str(content.text)
        #text = (content[:32000] + '..') if len(content) > 32000 else content
        # data.append([text])
        with io.open(str(i)+'.txt', 'a', encoding='utf8') as speech:
            speech.write(content.text)
        if i==499:
            break



