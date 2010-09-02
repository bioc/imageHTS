<html>
<head>
<style>
a.imgaction:link {color: black; font-size:9pt ; text-decoration: none; padding:0px ; background: white}
a.imgaction:visited {color: black; font-size:9pt ; text-decoration: none; padding:0px ; background: white}
a.imgaction:hover {background:#ffdc98}
</style>
</head>
<body>

<H1>Query images</H1>

<div class="slidecontainer">
  <div class="iconbox2contents" id="1_content"  style="display: block;">
    <form action="./" method="get">
      <INPUT type="submit" id="submit" value="Search">
      <span class="greenbold">&nbsp;for well, controlStatus or HGNC symbol: </span>
        <INPUT type="text" name="search" size="36" value="<?php echo $_GET["search"]; ?>">
  &nbsp;&nbsp;&nbsp;&nbsp;(ex: 002-01-B12, C17, rluc, BLK, ...)
</form>
</div>
</div>

<?php
include 'conf.php';
  $search = "";
  $dist = FALSE;
  if (isset($_GET["search"])) $search = $_GET["search"];
  if (isset($_GET["dist"])) $dist = $_GET["dist"];

  if ($search=="") die();

  $search = strtoupper($search);
  
  function lineAtPos($pos, $text) {
    $end = strpos($text, "\n", $pos + 1);
    if ($end === false) $end = strlen($text);
    $found = false;
    while (!$pos >= 0 && !$found) {
      if (substr($text, $pos, 1) == "\n") $found = true;
      else $pos -= 1;
    }
    if ($pos < 0) $pos = -1;
    return substr($text, $pos + 1, $end - $pos - 1);
  }
  
  $annot = file_get_contents("annotation-webquery.txt");

  $searchexplode = explode(" ", $search);
  $hits = array();
  $pos = -1; 
  $counter = 0;
  foreach ($searchexplode as $search) {
    while (($pos=strpos($annot, trim($search), $pos+1))!==false) {
      array_push($hits, lineAtPos($pos, $annot));
      $counter++;
    }
  }
  $hits=array_unique($hits);

  echo "<H2>Results</H2>";
  echo "<table border='1' style='border-collapse:collapse;' cellpadding='5px'>";
  echo "<tr><th>Uname</th><th>controlStatus</th><th>GeneID</th>";
  for ($i = 1; $i <= $nbreplicates; $i++) echo("<th>Replicate ".vsprintf("%02d", $i)."</th>");
  echo("</tr>");

  $counter = 0;  
  foreach ($hits as $hit) {
    $x = split("\t",$hit); 
    $uname = $x[0];
    $uname0 = $uname;
    $controlstatus = $x[1];
    $geneid = $x[2];
    $geneidlink = "http://harvester.fzk.de/cgi-bin/h4human/search.cgi?zoom_query=".$geneid;
    $cellpicker_pre = "../cellpicker.html?plabel=X&display=";
   
    echo "<tr>";
    echo "<td>".$uname."</td>";
    echo "<td class='leftsubheading'>".$controlstatus."</td>";
    echo "<td><a href='".$geneidlink."'>".$geneid."</a></td>\n";

    if ($counter<512) {
      for ($i = 1; $i <= $nbreplicates; $i++) {
      echo("<td class='leftsubheading'><table><tr>");
      $uname = substr_replace($uname, vsprintf("%02d", $i) , 4, 2);
      $subdir = "../view/".substr($uname, 0, 6);
      $viewfull = $subdir."/".$uname."_full.jpeg";
      $viewthumb = $subdir."/".$uname."_thumb.jpeg";
      $viewseg = $subdir."/".$uname."_seg.jpeg";
      $dataseg = "../data/".substr($uname, 0, 6)."/".$uname."_ftrs.tab";
   
      echo("<td><a href='".$viewfull."'><img src='".$viewthumb."' border=0 /></a></td>");
      echo("<td valign='bottom'><a class='imgaction' href='".$viewseg."'> seg </a><br>");
      echo("<a class='imgaction' href='".$dataseg."'> data </a><br>");
      echo("<a class='imgaction' href='".$cellpicker_pre);
      for ($j = 1; $j <= $montagex*$montagey; $j++) {
        if ($j != 1) echo("+");
	echo($uname."-".$j);
      }
      echo("' target='_blank'> anno </a></td>");

      echo("</tr></table></td>");
      }
    } else echo "<td class='leftsubheading'>Too many hits. No preview.</td>";

    $counter++;
    echo "</tr>\n";
  }
  echo "</table>";  
  ?>
    
</body>
</html>


