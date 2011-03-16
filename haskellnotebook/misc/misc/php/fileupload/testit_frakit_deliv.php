<?php
	session_start(); 
	
	////////////////////////////////////
	// Berlin Brown - file upload PHP (PSUEDO CODE EXAMPLE)
	// Date: 2/2/2009
	////////////////////////////////////
	
	//////////////////////////////////////////////////////////////
	// Check the args that are input from the previous form page.
	//////////////////////////////////////////////////////////////	
 	$str_page     = $_SESSION['testit_frakit_pw_page']; 
 	$str_resource = $_SESSION['testit_frakit_pw_resource'];
				
	// Tutorial page 
	if    ((md5($str_page) === '32900a23239023902803bd322') 
		&& (md5($str_resource) === '9238032392839089092830923')) {
		// Continue		 				    
	} else {
		exit;	
	}
	
?><!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en" >
 <head>
 	<title>Botnode</title>
 	
 	 <style type="text/css">

		 body {
		 	margin: 0px 0px 0px 0px;
			font-family: arial,helvetica,verdana,geneva,sans-serif;
			font-size: 12pt;
		 }
 	</style>
 	
 	
 </head>
 <body>
 
 	
 	<form method="post" action="testit_frakit_for_delivery.php" enctype="multipart/form-data"> 	
   		<p>
      	<label for="file">Select a Frak Deliv:</label> 
      	   	<input type="file" name="userfile" id="file"> 
      	   	<br />
      		<input type="submit" value=" Send Fraks " />
   		</p>
	</form>
 	
 
 </body>
</html>
 