(:---------------------------------------------------------------

Various XQueries that test database connectivity

----------------------------------------------------------------:)

declare function q ($n,$q) { (concat("Query ",$n,":\n"), $q, "\n") }
;
q(1,publish('hxq','c')//gradstudent[.//lastname='Galanis']/gpa)
;
q(2,for $s in publish('hxq','c')//gradstudent where $s//lastname='Galanis' return $s//gpa)
;
q(3,publish('hxq','c')//department[deptname]//gradstudent[.//lastname='Galanis']/gpa)
;
q(4,publish('hxq','c')//department[deptname='Computer Sciences']//gradstudent[.//lastname='Galanis']/gpa)
;
q(5,publish('hxq','c')//department/*[.//lastname='Galanis']/gpa)
;
q(6,publish('hxq','c')//*[.//lastname='Galanis']/gpa)
;
q(7,publish('hxq','c')//department[deptname]/deptname)
;
q(8,publish('hxq','c')//gradstudent[.//lastname='Galanis']/../deptname)
;
q(9,for $s in publish('hxq','c')/department/gradstudent return $s/../deptname)
;
q(10,publish('hxq','d')//article[author='Leonidas Fegaras']//title)
;
q(11,publish('hxq','d')//inproceedings[author='David Maier'][booktitle='SIGMOD Conference']/title)
;
q(12,<result>{
     for $x in publish('hxq','d')//inproceedings
     where $x/author = 'Leonidas Fegaras'
     order by $x/year descending
     return <paper>{ $x/booktitle/text(),
                     ':', $x/title/text()
            }</paper>
   }</result>)
