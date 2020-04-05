#Zaimplementuj jeszcze z biblioteką scrapy, bo inaczej serwer będzi blokował
import requests
from bs4 import BeautifulSoup

url = 'https://www.gumtree.pl/s-mieszkania-i-domy-sprzedam-i-kupie/warszawa/page-%d/v%dc9073l3200008p%d'
data = {'title': [], 'link':[]}
    
for i in range(1,51):
    page = requests.get(url  % (i,i,i))
    soup = BeautifulSoup(page.content, 'html.parser')
    titles = [flat.next_element for flat in soup.find_all('a', class_ = "href-link tile-title-text")] 
    links = ['https://www.gumtree.pl' + link.get('href')
                for link in soup.find_all('a', class_ ="href-link tile-title-text")]
    data['link'].extend(links)
    data['title'].extend(titles)
    
import pandas as pd
df = pd.DataFrame(data).drop_duplicates()
df.head(100)
df.to_csv("./gumtree_all_pages_30_03.csv", sep=';',index=False, encoding = 'utf-8')
#df.to_excel('./gumtree_page_1.xlsx')