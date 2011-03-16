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
				
	// Tutorial page 
	if    ((md5($str_page) === '387992c0d97f82a203223e67c6d184620f6bd322') 
		&& (md5($str_resource) === '999897897890a8a6fe2ab11d22cb7697ea3d3233290a02a660')) {
		 				    
	} else {
		exit;	
	}	
		
	// Configuration - Your Options
	// These will be the types of file that will pass the validation
	$allowed_filetypes = array('.txt', '.html', '.xhtml');
	
	 // Maximum filesize in BYTES (currently 0.5MB).
	$max_filesize = 224288;
	 // The place the files will be uploaded to (currently a 'files' directory).
	$upload_path = './var/test/frakhits/';
	
	// Get the name of the file (including file extension).
   	$filename = $_FILES['userfile']['name']; 
   	$ext = substr($filename, strpos($filename,'.'), strlen($filename)-1);
 
   	// Check if the filetype is allowed, if not DIE and inform the user.
   	if (!in_array($ext,$allowed_filetypes)) {
		die('Error (1).');
	}
 
	// Now check the filesize, if it is too large then DIE and inform the user.
	if (filesize($_FILES['userfile']['tmp_name']) > $max_filesize) {
		die('Error (2).');
	}
 
	// Check if we can upload to the specified path, if not DIE and inform the user.
	if (!is_writable($upload_path)) {
		//die('You cannot upload to the specified directory, please CHMOD it to 777.');
		die('Error (3).');
	}
 
	// Handle Upload
	if (move_uploaded_file($_FILES['userfile']['tmp_name'],$upload_path . $filename)) {
		echo 'Your file upload was successful, view the file <a href="' . $upload_path . $filename . '" title="Your File">here</a>';
	} else {
		echo 'There was an error (4).';
	} 		
	// Destroy the session we don't need it anymore.	 
	session_destroy();	
 
?>