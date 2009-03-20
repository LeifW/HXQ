(:---------------------------------------------------------------

Various XQueries tests. There results are in test-results.txt

----------------------------------------------------------------:)

declare function q ($n,$q) { (concat("Query ",$n,":\n"), $q, "\n") }
;
q(1,(1 to 100)[10])
;
q(2,(1 to 100)[last()])
;
q(3,(1 to 100)[position()<4])
;
q(4,(1 to 100)[.<4])
;
q(5,(1 to 100)[.>3 and .<10])
;
q(6,<a>{1}2<b>{3+1,4}5{6*3}</b></a>)
;
q(7,<a x="1{2,5}3{3+1}"/>)
;
q(8,count(1 to 100))
;
q(9,sum(1 to 100))
;
q(10,avg(1 to 100))
;
q(11,contains("abcde","cd"))
;
q(12,contains("abcde","ce"))
;
q(13,if 1<2 then if 3>4 then 5 else 6 else 7)
;
q(14,<z>{for $a in (1,2,3) return <a>{$a}</a>}</z>)
;
q(15,for $v in (1,2,3,4) where not($v < 3) return $v)
;
q(20,for $x in (1,2,3), $y in (4,5) return $x+$y)
;
q(21,for $x in (1,2,3), $y in (4,5) return <a>{$x,$y}</a>)
;
q(24,(<A><a><b>1</b></a><a><b>2</b></a></A>)/a/b)
;
q(24.1,(<a><b>1</b></a>)//b[.=1])
;
q(25,(<A><a><b>1</b></a><a><b>2</b></a></A>)//*[b="2"])
;
q(27,<a>{
  for $a in (1,2) return $a+10,
  for $b in (3,4) return $b+20
}</a>)
;
declare function f ($x,$y) { <a>{$x,$y}</a> };
q(28,f(1,2))
;
declare function g ($x,$y) { $x*$y };
declare function f ($x,$y) { $x+$y };
q(29,g(f(5,g(6,f(3,1))),2))
;
declare function f ($x,$y) { $x+$y*2 };
declare function g ($x,$y) { f($x,$y)*3-f($y,$x) };
q(30,g(4,5))
;
declare function fact ($n) { if $n <= 1 then 1 else $n*fact($n - 1) };
q(31,fact(1))
;
q(32,fact(10))
;
declare function f ($x) { for $v in $x return <a>{$v}</a> };
q(33,f((1,2,3)))
;
q(34,doc("data/c.xml")/a/@x)
;
q(35,doc("data/c.xml")/a/@*/string())
;
q(36,doc("data/c.xml")//@*/name())
;
q(37,doc("data/a.xml")/a/b[d = "K6"])
;
q(38,doc("data/c.xml")/a/b[e]/d)
;
q(39,doc("data/c.xml")/a[b/e]/n)
;
q(40,doc("data/c.xml")/a/b[e and d]/c)
;
q(40.1,doc("data/c.xml")/a/b[f | d]/c)
;
q(41,(doc("data/a.xml")/a/*/d)[3])
;
q(42,doc("data/a.xml")/a/b[c="test3"])
;
q(43,doc("data/a.xml")/a/b[d="K3"])
;
q(44,doc("data/a.xml")/a/*[d="K1"]/c/text())
;
q(45,doc("data/a.xml")/a/b[c="test3"][d="K3"]/c)
;
q(46,doc("data/a.xml")/a/b[c="test3" and d="K3"]/c)
;
q(47,doc("data/a.xml")/a/b[not(c="test5") and d="K3"]/c)
;
q(48,doc("data/a.xml")/a/b[d="K3"][c="test3"]/*)
;
q(49,doc("data/a.xml")//*[d="K3"]/d)
;
q(50,doc("data/a.xml")//*[d="K3"]/c)
;
q(51,doc("data/a.xml")/a/b[d="K3"]//*)
;
q(52,doc("data/c.xml")//b[d="4"]//*)
;
q(53,for $x in doc("data/a.xml")/a/b/c return $x)
;
q(54,for $x in doc("data/a.xml")/a/b return ($x/c,$x/d))
;
q(55,doc("data/c.xml")/a/b/e/../c)
;
q(56,doc("data/a.xml")/a/b[d="K3"]/c/..)
;
q(57,doc("data/a.xml")/a/b/d[. = "K6"]/..)
;
q(58,for $v in doc("data/a.xml")/a/b
     where $v/d="K3"
     return ($v/d,$v/d))
;
q(59,for $x in doc("data/a.xml")/a/b
     where $x/c = "test3"
     return ($x/d,"@"))
;
q(60,for $x in doc("data/a.xml")/a/b[c = "test3"]
     return ($x/d,"@"))
;
q(61,for $x in doc("data/c.xml")/a/b
     where $x/c = "test3"
     return $x/d+1)
;
q(62,for $x in doc("data/c.xml")/a/b[c = "test3"]
     return $x/d+1)
;
q(63,<a>{
  for $x in doc("data/a.xml")/a/b
  return $x/c,
  for $a in (1,2,3) return $a
}</a>)
;
q(64,<result>{
  for $x in doc("data/a.xml")/a/b
  where $x/c = "test3"
  return $x/d
}</result>)
;
q(65,for $v in doc("data/a.xml")/a/b,
    $w in $v/c,
    $z in $v/d
return <k>{$v/d,$w,$z}</k>)
;
q(66,for $v in doc("data/a.xml")/a/b,
    $w in $v/c,
    $z in $v/d
return <k>{$w,$z}</k>)
;
q(67,<result>{
 for $v in doc("data/a.xml")/a/b
 return <a>{$v/c,$v/d}</a>
}</result>)
;
q(68,for $v in doc("data/a.xml")/a/b
     where $v/d="K5" and $v/c="test5"
     return $v/c)
;
q(69,for $v in doc("data/a.xml")/a/b
     where $v/d="K3"
     return <a>{ $v/c, for $w in $v/c return $w/text() }</a> )
;
q(70,for $v in doc("data/a.xml")/a/b
     where $v/d="K3"
     return <a>{ for $w in $v/c return $w/text(),
                 for $w in $v/d return $w/text() 
            }</a> )
;
q(71,some $v in doc("data/a.xml")/a/b
     satisfies $v/d="K3" and $v/c="test5")
;
q(72,every $v in doc("data/a.xml")/a/b
     satisfies $v/d="K3" or $v/c="test5")
;
q(73,for $v in (3,2,6,4,7,8,2,4,6,4)
     order by $v
     return $v)
;
q(74,for $v in doc("data/a.xml")/a/b
     order by $v/d
     return $v/c/text())
;
q(75,for $v in doc("data/a.xml")/a/b
     order by $v/d descending, $v/c
     return $v/c/text())
;
q(76,for $v in doc("data/a.xml")/a/b
     return count($v/c))
;
q(77,for $v in doc("data/a.xml")/a/b
     order by count($v/c)
     return ($v/d,count($v/c)))
;
q(78,for $v in doc("data/c.xml")/a/b/d
     for $w in doc("data/c.xml")/a/b/d
     return ($v/text(),$w/text()," "))
;
q(80,for $s in doc("data/cs.xml")//gradstudent
     where $s//firstname="Leonidas"
     return $s/gpa)
;
q(81,for $s in doc("data/cs.xml")//gradstudent
     order by $s/gpa descending
     return ($s//firstname/text()," ",$s//lastname/text()," ",$s/gpa/text(),"\n"))
;
declare function f ($x,$y) { <a>{$x,$y}</a> };
q(82,f(1,2))
;
q(83,for $v in doc("data/a.xml")/a/b
     where some $w in $v/c
           satisfies $w="test2"
     return $v/d)
;
q(84,let $x := doc("data/a.xml")/a/b,
         $y := "K3"
     return $x[d = $y]/c)
;
q(85,(for $v in doc("data/a.xml")/a/b
      for $w in doc("data/a.xml")/a/b
      where $v/d=$w/d
      return <result>{$v,$w}</result>)[2])
;
declare variable $xxx := doc('data/cs.xml')
;
declare function x:f ($d) { $xxx//gradstudent[.//lastname=$d]/gpa }
;
declare function g ($x) { ($x,for $z in $x/* return g($z)) }
;
q(86,(x:f("Galanis"),count(g($xxx))))
;
declare function local:copy($element) {
   element {name($element)}
      {$element/@*,
          for $child in $element/node()
              return
               if ($child/name())
                 then local:copy($child)
                 else $child
      }
}
;
q(87,local:copy(doc("data/a.xml"))//d/string())
