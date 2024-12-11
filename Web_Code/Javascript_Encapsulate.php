<?php
class HTML_Outputer
{      
   public function HTML_Make($Title       = false,
                             $In_bit      = false)
   {
      //
      // Set up some static HTML to hold the metadata and CSS links
      //;  

      $Meta_Data  = '<meta charset="utf-8">
                     <meta name="viewport" content="width=device-width, initial-scale=1">
                     <meta name="description" content="">
                     <meta name="author" content="Mark Otto, Jacob Thornton, and Bootstrap contributors">
                      <meta name="generator" content="Hugo 0.84.0">';
                      
      $CSS_Links  = '<link rel="canonical" href="https://getbootstrap.com/docs/5.0/examples/cover/">
                     <link href="../../Web_Skeleton/css/bootstrap.min.css" rel="stylesheet">
                     <link href="../../Web_Skeleton/css/custom.css" rel="stylesheet" />';
                            
      $Styles     = '<style>
                       .bd-placeholder-img {
                         font-size: 1.125rem;
                         text-anchor: middle;
                         -webkit-user-select: none;
                       -moz-user-select: none;
                         user-select: none;
                       }

                       @media (min-width: 768px) {
                         .bd-placeholder-img-lg {
                          font-size: 3.5rem;
                         }
                       }
                    </style>';

      $Javascript_Content  = '<script src="../../D3Play/js/bootstrap.min.js"></script>
                              <style>@import url("https://fonts.googleapis.com/css2?family=Allison&family=Mukta+Malar:wght@200&display=swap");
                                     @import url("https://fonts.googleapis.com/css2?family=Montserrat:wght@200&display=swap");</style>
                             <script src="../../D3Play/js/d3.v6.min.js"></script>';
           
    //
    //      Get the content passed to the function
    //;
      $this->Title              = $Title;     
      $this->In_bit             = $In_bit;     
      
      $Head_HTML  = '<!DOCTYPE html>
                        <html lang="en" class="h-100">
                           <head>';
      $Head_HTML .= '<title>'.$this->Title.'</title>
      
            ';
      $Head_HTML .= $Meta_Data;
      $Head_HTML .= $Styles;
      $Head_HTML .= $Javascript_Content;
      $Head_HTML .= $CSS_Links.'</head>';
      $Head_HTML .= $this->In_bit;
      
      return $Head_HTML;      
   }
}

class NavBar_Maker
{

   public function Header_Content()
   {   
      $Content  = '<header class="mb-auto">
       <div>
         <h3 class="float-md-start mb-0">Cover</h3>
         <nav class="nav nav-masthead justify-content-center float-md-end">
           <a class="nav-link active" aria-current="page" href="#">Home</a>
           <a class="nav-link" href="#">Features</a>
           <a class="nav-link" href="#">Contact</a>
         </nav>
       </div>
     </header>';
     
     return $Content;
   }

   public function Footer_Content()
   {   
      $Content  = '<footer class="mt-auto text-white-50">
    <p>Cover template for <a href="https://getbootstrap.com/" class="text-white">Bootstrap</a>, by <a href="https://twitter.com/mdo" class="text-white">@mdo</a>.</p>
  </footer>';
     
     return $Content;
   }


}

?>











