<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->
<!-- $Id$ -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
	<link href="additions.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<h3>what is this page?</h3>
<ul>
<li>if don't know what is a FEWS general adapter then you are probably looking at the wrong page.</li>
<li>if you know that but don't know how to use this library and what it can do for you, read further.</li>
</ul>
<h3>introduction</h3>
<ul>
<li>aim of the <tt>delftfews</tt> package is to help you writing FEWS general adapters in R.</li>
</ul>
<h3>background information</h3>
<ul>
<li>FEWS (Flood Early Warning System) at <a href="http://www.deltares.nl/en/searchresults?search-expression=fews&page=1">deltares.nl</a></li>
<li>description of the exchange format PI (public interface) at <a href="http://publicwiki.deltares.nl/display/FEWSDOC/The+Delft-Fews+Published+interface+%28PI%29">deltares.nl</a>.</li>
</ul>

<h3>technical information</h3>
<ul>
<li>The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>.</li>
<li>The <strong>released packages</strong> you can find <a href="http://r-forge.r-project.org/R/?group_id=775"><strong>here</strong></a>.</li>
</ul>

</body>
</html>
