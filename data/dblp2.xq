for $x in doc('data/dblp.xml')//inproceedings
where $x/author = 'David Maier'
return <paper>{ $x/booktitle/text(),
                ': ', $x/title/text()
       }</paper>
