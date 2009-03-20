<result>{
     for $x in publish('hxq','d')//inproceedings
     where $x/author = 'Leonidas Fegaras'
     order by $x/year descending
     return <paper>{ $x/booktitle/text(),
                     ':', $x/title/text()
            }</paper>
}</result>
