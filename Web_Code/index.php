<?php

session_start();

//
//    Load up some functionality
//
   include("Javascript_Encapsulate.php");
   $Title  = 'Play with Bootstrap and D3';

//
//    Initialise some =objets
//

   $Send_to_Web  = new HTML_Outputer();
   $NavBar_Maker = new NavBar_Maker();


//
//    Put in some content that replicates the html template file
//

     $Content  =  '<body class="d-flex h-100 text-center text-white bg-dark">
                     <div class="cover-container d-flex w-100 h-100 p-3 mx-auto flex-column">';
                     
     $Content .= $NavBar_Maker ->Header_Content();

     $Content .= '<main class="px-3">
                         <h1>Cover your page.</h1>
                         <p class="lead">Cover is a one-page template for building simple and beautiful home pages. Download, edit the text, and add your own fullscreen background photo to make it your own.</p>
                         <p class="lead">
                           <a href="#" class="btn btn-lg btn-secondary fw-bold border-white bg-white">Learn more</a>
                         </p>
                       </main>';

     $Content .= $NavBar_Maker ->Footer_Content();
     $Content .= '</div></body>'; 
                  
//
//   Output to Website
//
     print($Send_to_Web->HTML_Make($Title,
                                   $Content)); 

 ?>

