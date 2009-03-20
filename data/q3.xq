<result>{
let $a := distinct-values(doc("data/cs.xml")//gradstudent)
for $s in $a
return <st>{
    doc("data/cs.xml")//gradstudent[.//lastname = $s//lastname]//name,
    count(.//*)
    }</st>
}</result>
