declare function fact ($n) { if $n=1 then 1 else $n*fact($n - 1) };

declare variable $x := fact(10);

declare function f ( $name, $gpa ) {
   <student>{ $name/firstname/text(),
              $name/lastname/text()
            }: {$gpa}</student>
};

<students>{
    for $s in doc('data/cs.xml')//gradstudent
    order by $s/gpa descending, $s//lastname
    return f( $s/name, $s/gpa/text() ),
    $x
}</students>
