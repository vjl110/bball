import urllib2
import csv
import re

def scrape_schools():
	url="http://www.sports-reference.com/cbb/schools/"
	page=urllib2.urlopen(url).read()

	school_finder=re.compile('<td align="left" ><a href="(/cbb/schools/.*?/)">(.*?)</a></td>')
	school_urls=school_finder.findall(page)

	final_seasons=[]

	for url in school_urls:
		url2="http://www.sports-reference.com%s" % (url[0])
		history=urllib2.urlopen(url2).read()
		season_finder=re.compile('<tr  class="valign_top">\n   <td align="right"  csk=".*?">([0-9]{1,3})</td>\n   <td align.*?>([0-9]{4}-[0-9]{2}).*?/td>\n   <td align="left" ><a href="/cbb/conferences/.*?" title=".*?">(.*?)</a></td>\n   <td align="right" >(.*?)</td>\n   <td align="right" >(.*?)</td>\n   <td align="right" >(.*?)</td>\n   <td align="right" >(.*?)</td>\n   <td align="right" >(.*?)</td>\n   <td align="right" >(.*?)</td>\n   <td align="right" >(.*?)</td>\n   <td align="center"  csk=".*?">(.*?)</td>\n   <td align="center" >(.*?)</td>\n   <td align="center"  csk=".*?">(.*?)</td>\n   <td align="left".*?>(.*?)</td>\n   <td align="left" >(?:(No coach)|<a href=".*?">(.*?)</a>)')
		seasons=season_finder.findall(history)

		seasons2=[]

		for season in seasons:
			sea2=[]
			sea2.append(url[1])
			for i,obj in enumerate(season):
				if i==14 and obj!="No coach":
					pass
				else:
					sea2.append(obj)
			seasons2.append(sea2)

		final_seasons.append(seasons2)

	with open('school_histories.csv','a') as csvfile:
		writer=csv.writer(csvfile)
		writer.writerow(["Team","Rk","Season","Conf","W","L","W-L%","SRS","SOS","PTS","PTS","AP Pre","AP High","AP Final","NCAA Tournament","Coach"])
		for team in final_seasons:
			for season in team:
				writer.writerow(season)
