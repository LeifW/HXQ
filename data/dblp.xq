<result>{
     for $x at $i in doc('data/dblp.xml')//inproceedings
     where $x/author = 'Leonidas Fegaras'
     order by $x/year descending
     return <paper>{ $i, ') ', $x/booktitle/text(),
                     ': ', $x/title/text()
            }</paper>
  }</result>
