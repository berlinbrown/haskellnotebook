<?php
	session_start(); 
	
	////////////////////////////////////
	// Berlin Brown - file upload PHP (PSUEDO CODE EXAMPLE)
	// Date: 2/2/2009
	////////////////////////////////////
		
	
	//////////////////////////////////////////////////////////////
	// Check the args that are input from the previous form page.
	//////////////////////////////////////////////////////////////
	$str_user     = $_POST['frakInfo']; 	
 	$str_resource = $_POST['frakInfoPage'];
				
	// Tutorial page 
	if    ((md5($str_user) === '923490820938323230230238900517') 		
		&& (md5($str_resource) === '2934289348203809283908929372893')) {
				 
		$_SESSION['testit_frakit_flowers_ready'] = 'true';				    
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
 
    You are ready to deliver flowers.
    
    <a href="testit_frakit_deliv.php">Frak Delivery</a> 	 	 		
 
 </body>
</html>
 