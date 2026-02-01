SPLICE: dialog {
    label = "Splice draw";
    : image {
      key = "dd_splice";
      aspect_ratio = 1.00;
      color = -2;
      width = 30;
    }
    : row {
      : edit_box {
        label = "N1";
        key = "n1";
        edit_width = 3;
        alignment = right;
      }
      : edit_box {
        label = "P1";
        key = "p1";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "E1";
        key = "e1";
        edit_width = 6;
        alignment = right;
      }
      : text {
        label = "=>";
      }
      : text {
        key = "total1";
        width = 6;
        horizontal_alignment = right;
        vertical_alignment = centered;
      }
    }
    : row {
      : edit_box {
        label = "N2";
        key = "n2";
        edit_width = 3;
        alignment = right;
      }
      : edit_box {
        label = "P2";
        key = "p2";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "E2";
        key = "e2";
        edit_width = 6;
        alignment = right;
      }
      : text {
        label = "=>";
      }
      : text {
        key = "total2" ;
        width = 6 ;
        horizontal_alignment = right ;
        vertical_alignment = centered ;
      }
    }
    : row {
      : edit_box {
        label = "G " ;
        key = "g" ;
        edit_width = 6 ;
      }
      : spacer { width = 5; }
      : text {
        label = "distance = " ;
      }
      : text {
        key = "dist" ;
        width = 8 ;
      }
    }
    spacer;
    ok_cancel_err;
}

txtedit : dialog {
    label = /*MSG180*/"Edit Text";
    initial_focus = "text_edit";
    : edit_box {
        label = /*MSG181*/"Text:";
        key = "text_edit";
        edit_width = 40;
        edit_limit = 255;
        allow_accept = true;
    }
    ok_cancel;
}

nvdialog : dialog {
    label = /*MSG180*/"Edit New Value";
    initial_focus = "value_edit";
    :row {
    : edit_box {
//        label = /*MSG181*/"Text:";
        key = "tag_edit";
        edit_width = 3;
        edit_limit = 255;
        allow_accept = true;
    }
    : text    {
      label = "=";
    }
    : edit_box {
//        label = /*MSG181*/"Text:";
        key = "value_edit";
        edit_width = 12;
        edit_limit = 255;
        allow_accept = true;
    }
    }
    ok_cancel;
}




scl : dialog {
      label = "Initialize Drawing" ;
      : boxed_row {
        label = "OPTIONS" ;
        : edit_box {
          label = "SCALE  1 :" ;
          key = "user_scl" ;
          edit_width = 6 ;
        }
        : spacer { width = 4 ; }
        :toggle {
          label = "1Unit=1m";
          key = "grd";
          mnemonic = "U";
        }
      }
      spacer;
      : boxed_column {
        label = "BORDER INSERT" ;
        : boxed_radio_row {
          label = "BORDER TYPE" ;
          : radio_button {
            key = "x_border" ;
            label = "Xref Border" ;
            mnemonic = "X" ;
          }
          : radio_button {
            key = "b_border" ;
            label = "Block Border" ;
            mnemonic = "B" ;
          }
          : radio_button {
            key = "n_border" ;
            label = "None" ;
            mnemonic = "N" ;
          }
        }
        spacer;
        : edit_box {
          label = "BODER" ;
          key = "bdr_file" ;
          edit_width = 30 ;
        }
    }
      spacer;
      ok_cancel_err;
    }


PIER: dialog {
    label = "PIER draw";
    : image {
      key = "dd_pier";
      aspect_ratio = 0.8;
      color = -2;
      width = 30;
    }
    : row {
        : edit_box {
          label = "C1";
          key = "c1";
          edit_width = 6;
          alignment = right;
        }
        : edit_box {
          label = "C2";
          key = "c2";
          edit_width = 6;
          alignment = right;
        }
        : edit_box {
          label = "CB";
          key = "bc";
          edit_width = 6;
          alignment = right;
        }
      }
    : row {
        : edit_box {
          label = "PD";
          key = "pd";
          edit_width = 6;
          alignment = right;
        }
        : edit_box {
          label = "PL";
          key = "pl";
          edit_width = 6;
          alignment = right;
        }
       : edit_box {
         label = "FB";
         key = "fb";
         edit_width = 6;
         alignment = right;
       }
    }
    : row {
      : edit_box {
        label = "FH";
        key = "fh";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "       Y-Scale";
        key = "ys";
        edit_width = 6;
        alignment = right;
      }
    }
    : row {
    }
    : row {
      : edit_box {
        label = "          Slop of coping 1:";
        key = "scoping";
        edit_width = 6;
        alignment = right;
      }
    }
    : row {
      :toggle {
        label = "Round coping";
        key = "rcoping";
        mnemonic = "R";
      }
      :toggle {
        label = "Dimension";
        key = "dim";
        mnemonic = "D";
      }
    }
    spacer;
    ok_cancel_err;
}

ABUT: dialog {
    label = "ABUT draw";
    : image {
      key = "dd_abut";
      aspect_ratio = 1.0;
      color = -2;
      width = 30;
    }
    : row {
      : edit_box {
        label = "H1";
        key = "h1";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "H2";
        key = "h2";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "H3";
        key = "h3";
        edit_width = 6;
        alignment = right;
      }
    }
    : row {
      : edit_box {
        label = "H4";
        key = "h4";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "H5";
        key = "h5";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "H6";
        key = "h6";
        edit_width = 6;
        alignment = right;
      }
    }
    : row {
      : edit_box {
        label = "B1";
        key = "b1";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "B2";
        key = "b2";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "B3";
        key = "b3";
        edit_width = 6;
        alignment = right;
      }
    }
    : row {
      : edit_box {
        label = "B4";
        key = "b4";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "B5";
        key = "b5";
        edit_width = 6;
        alignment = right;
      }
      : edit_box {
        label = "B6";
        key = "b6";
        edit_width = 6;
        alignment = right;
      }
    }
    : row {
      : edit_box {
        label = "Y-Scale";
        key = "ys";
        edit_width = 6;
        alignment = right;
      }
      : spacer { width = 5 ; }
      :toggle {
        label = "Dimension";
        key = "dim";
        mnemonic = "D";
      }
    }
    spacer;
    ok_cancel_err;
}

SB: dialog {
    label = " Split Rebar Draw";
    : image {
      key = "sb";
      aspect_ratio = 0.73024;
      color = -2;
      width = 30;
    }
    : row {
      : edit_box {
        label = "N1";
        key = "n1";
        edit_width = 2;
        alignment = right;
      }
      : spacer { width = 5 ; }
      : edit_box {
        label = "P1";
        key = "p1";
        edit_width = 3;
        alignment = right;
      }
      : spacer { width = 5 ; }
      : text {
        label = "=>";
      }
      : text {
        key = "total1";
        width = 6;
        horizontal_alignment = right;
        vertical_alignment = centered;
      }
    }
    : row {
      : edit_box {
        label = "N2";
        key = "n2";
        edit_width = 2;
        alignment = right;
      }
      : spacer { width = 5 ; }
      : edit_box {
        label = "P2";
        key = "p2";
        edit_width = 3;
        alignment = right;
      }
      : spacer { width = 5 ; }
      : text {
        label = "=>";
      }
      : text {
        key = "total2" ;
        width = 6 ;
        horizontal_alignment = right ;
        vertical_alignment = centered ;
      }
    }
    : row {
      : edit_box {
        label = "EX" ;
        key = "ex" ;
        edit_width = 3 ;
      }
      : spacer { width = 5; }
      : edit_box {
        label = "SCALE" ;
        key = "scale" ;
        edit_width = 2 ;
      }
      : spacer { width = 5 ; }
    }
    : row {
      : edit_box {
        label = "M1";
        key = "m1";
        edit_width = 5;
        alignment = left;
      }
      : spacer { width = 5; }
      : edit_box {
        label = "D1";
        key = "d1";
        edit_width = 3;
        alignment = left;
      }
      : spacer { width = 5 ; }
    }
    : row {
      : edit_box {
        label = "M2";
        key = "m2";
        edit_width = 5;
        alignment = left;
      }
      : spacer { width = 5; }
      : edit_box {
        label = "D2";
        key = "d2";
        edit_width = 3;
        alignment = left;
      }
      : spacer { width = 5 ; }
    }
    spacer;
    ok_cancel_err;
}

SBLOCK: dialog {
      label = " Shoe Block Draw";
    : row {
      : column {
        : image {
          key = "sblock";
          aspect_ratio = 0.73024;
          color = -2;
          width = 30;
        }
      }
      : column {
        : row {
          : edit_box {
            label = "X1" ;
            key = "x1" ;
            edit_width = 3 ;
          }
          : edit_box {
            label = "N1 ";
            key = "n1";
            edit_width = 2;
            alignment = right;
          }
          : edit_box {
            label = "P1";
            key = "p1";
            edit_width = 3;
            alignment = right;
          }
          : text {
            label = "=>";
          }
          : text {
            key = "total1";
            width = 6;
            horizontal_alignment = right;
            vertical_alignment = centered;
          }
        }
        : row {
          : edit_box {
            label = "X2" ;
            key = "x2" ;
            edit_width = 3 ;
          }
          : edit_box {
            label = "N2 ";
            key = "n2";
            edit_width = 2;
            alignment = right;
          }
          : edit_box {
            label = "P2";
            key = "p2";
            edit_width = 3;
            alignment = right;
          }
          : text {
            label = "=>";
          }
          : text {
            key = "total2" ;
            width = 6 ;
            horizontal_alignment = right ;
            vertical_alignment = centered ;
          }
        }
        : row {
          : edit_box {
            label = "A1";
            key = "a1";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "A2";
            key = "a2";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "A3";
            key = "a3";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "A4";
            key = "a4";
            edit_width = 4;
            alignment = left;
          }
        }
        : row {
          : edit_box {
            label = "B1";
            key = "b1";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "B2";
            key = "b2";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "N3 ";
            key = "n3";
            edit_width = 2;
            alignment = right;
          }
          : edit_box {
            label = "P3 ";
            key = "p3";
            edit_width = 3;
            alignment = right;
          }
        }
        : row {
          : edit_box {
            label = "M1";
            key = "m1";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "M2";
            key = "m2";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "M3";
            key = "m3";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "M4";
            key = "m4";
            edit_width = 3;
            alignment = left;
          }
        }
        : row {
          : edit_box {
            label = "D1";
            key = "d1";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "D2";
            key = "d2";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "D3";
            key = "d3";
            edit_width = 3;
            alignment = left;
          }
          : edit_box {
            label = "D4";
            key = "d4";
            edit_width = 3;
            alignment = left;
          }
        }
        : row {
          : spacer { width = 5; }
          : edit_box {
            label = "SCALE  1:" ;
            key = "scale" ;
            edit_width = 2 ;
          }
          : spacer { width = 5 ; }
        }
      }
    }
    spacer;
    ok_cancel_err;
}

DJDGLOGO: dialog {
    label = "About DJDG";
    : image {
      key = "djdglogo";
      aspect_ratio = 0.73024;
      color = -2;
      width = 30;
    }
    spacer;
    ok_only;
}

WELD : dialog {
    label = "Weld Mark";
        : row {
        : column {
            : image_button {
                key = "fu_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "ku_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "vu_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
        }
        : column {
            : image_button {
                key = "fl_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "kl_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "vl_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
        }
        : column {
            : image_button {
                key = "fb_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "kb_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "vb_weld";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
        }
        : column {
          : edit_box {
              label = "Weld Size:";
              key = "wsize";
              edit_width = 2;
              allow_accept = false;
          }
          : edit_box {
              label = "Groove Angle:";
              key = "gangle";
              edit_width = 2;
              allow_accept = false;
          }
          : edit_box {
              label = "Gap of root:";
              key = "groot";
              edit_width = 2;
              allow_accept = false;
          }
          : toggle {
              label = "Weld All Around";
              key = "round";
          }
          : toggle {
              label = "Field Weld";
              key = "field";
          }
        }
}
    spacer;
    ok_cancel;
}

RWALL: dialog {
      label = " Draw ReWall";
  : row {
    : column {
      : image {
        key = "rwall";
        aspect_ratio = 1.0;
        color = -2;
        width = 40;
      }
    }
    : column {
      : row {
        : column {
          : edit_box {
            label = "B1" ;
            key = "b1" ;
            edit_width = 6 ;
            alignment = right;
          }
          : edit_box {
            label = "B2 ";
            key = "b2";
            edit_width = 6;
            alignment = right;
          }
          : edit_box {
            label = "B3";
            key = "b3";
            edit_width = 6;
            alignment = right;
          }
          : edit_box {
            label = "B4";
            key = "b4";
            edit_width = 6;
            alignment = right;
          }
          : text {
            key = "totalb";
//            width = 6;
            horizontal_alignment = right;
            vertical_alignment = centered;
          }
          : edit_box {
            label = "B5";
            key = "b5";
            edit_width = 6;
            alignment = right;
          }
//          : text {
//            label = "B = ";
//          }
        }
      : column {
        : edit_box {
          label = "      H1" ;
          key = "h1" ;
          edit_width = 6 ;
          alignment = right;
        }
        : edit_box {
          label = "      H2 ";
          key = "h2";
          edit_width = 6;
          alignment = right;
        }
        : edit_box {
          label = "      H3";
          key = "h3";
          edit_width = 6;
          alignment = right;
        }
//        : text {
//          label = "H = ";
//        }
        : text {
          key = "totalh" ;
//          width = 6 ;
          horizontal_alignment = right ;
          vertical_alignment = centered ;
        }
        : edit_box {
          label = "Y-scale";
          key = "yscale";
          edit_width = 6;
          alignment = right;
        }
      }
    }
    :boxed_row{
      label = "Options";
      :toggle {
        label = "Front Slop 0.02";
        key = "fslop";
          mnemonic = "";
      }
      :toggle {
        label = "Dimension";
        key = "dim";
        mnemonic = "D";
      }
    }
    : text {
      label = "Load Data";
    }
    :row {
      :button {
        label = "File..";
        key   = "dfile";
        mnemonic = "F";
        width = 11.7;
        fixed_width = true;
      }
      :edit_box {
        key = "path_name";
        width = 36;
      }
    }
    : row {
      : button {
        label = "OK";
        key = "accept";
        is_default = true;
        width = 8;
        fixed_width = true;
        mnemonic = "O";
      }
      : button {
        label = "Reset";
        key = "reset";
        width = 8;
        fixed_width = true;
        mnemonic = "R";
      }
      : button {
        label = "Cancel";
        key = "cancel";
        is_cancel = true;
        width = 8;
        fixed_width = true;
        mnemonic = "C";
      }
    }
    errtile;
  }
 }
}


noripf : dialog {
      label = "Draw Nori (Profile)" ;

      : boxed_radio_row {
        label = "Meter or Milimeter" ;
        : radio_button {
          key = "meter" ;
          label = "Meter" ;
          mnemonic = "M" ;
        }
        : radio_button {
          key = "milimeter" ;
          label = "Milimeter" ;
          mnemonic = "i" ;
        }
      }
      spacer;
      
      :toggle {
        label = "Write Slop";
        key = "wslop";
        mnemonic = "S";
      }
      
      spacer;
      ok_cancel_err;
    }

para : dialog {
      label = "Draw parabola" ;
      : boxed_row {
        label = "Property of Parabola";
        : column {
          : boxed_radio_row {
            label = "Exponent" ;
            : radio_button {
              key = "exp2" ;
              label = "2" ;
              mnemonic = "X" ;
            } 
            : radio_button {
              key = "exp3" ;
              label = "3" ;
              mnemonic = "B" ;
            }
          }  
        : row {
          : text {
            key = "length" ;
          }
          : text {
            key = "height" ;
          }
        }

        : text {
          key = "prop" ;
        }
      }
      }
      : boxed_row {
        label = "Options" ;
        : edit_box {
          label = "Divid Num:" ;
          key = "div" ;
          edit_width = 6 ;
        }
        :toggle {
          label = "Both";
          key = "both";
          mnemonic = "B";
        }
      }
      :boxed_row {
        label = "Cap" ;
        :toggle {
          label = "Cap";
          key = "cap";
          mnemonic = "C";
        }
        : edit_box {
          label = "Length: " ;
          key = "cap_len" ;
          edit_width = 6 ;
        }
      }
      spacer;
      ok_cancel_err;
    }


bar : dialog {
      label = "Rebar List" ;
        : boxed_radio_row {
          label = "Roundup Type" ;
          : radio_button {
            key = "round4" ;
            label = "RoundUp 4" ;
            mnemonic = "4" ;
          }
          : radio_button {
            key = "round5" ;
            label = "RoundUp 5" ;
            mnemonic = "5" ;
          }
          : radio_button {
            key = "roundno" ;
            label = "None" ;
            mnemonic = "N" ;
          }
        }

//      :toggle {
//        label = "Trunc 3 Roundup 4";
//        key = "tr34";
//        mnemonic = "T";
//      }

      : edit_box {
        label = "Add Ratio (%)" ;
        key = "addratio" ;
        edit_width = 8 ;
      }

      
      : boxed_column {
//        label = "OPTIONS" ;
        :toggle {
          label = "Add by group";
          key = "addg";
          mnemonic = "G";
        }
        : edit_box {
          label = "D10~D19(%)" ;
          key = "adds" ;
          edit_width = 8 ;
        }
        : edit_box {
          label = "D22~D32(%)" ;
          key = "addb" ;
          edit_width = 8 ;
        }
      }
      
        : boxed_radio_row {
          label = "Output Type" ;
          : radio_button {key = "drawtbl" ;label = "Draw Table" ; mnemonic = "D" ;}
          : radio_button {key = "fileout" ;label = "File Out" ;mnemonic = "F" ;}
        }
      
      spacer;
      ok_cancel_err;
    }

SCALLOP : dialog {
    label = "Scallop";
    : row {
             height = 4;
            : image_button {
                key = "sc_none";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "sc_tensile";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "sc_compress";
                width = 6;
                fixed_height = 4;
                allow_accept = false;
                color = 5;
            }

    }
    : row {
        : edit_box {
          label = "Length";
          key = sc_length;
          edit_width = 4;
        }  
        : edit_box {
          label = "Thick";
          key = sc_thick;
          edit_width = 4;
        }  

    }
    spacer;
    ok_cancel;
}

PILES : dialog {
      label = "Draw parabola" ;

        : row {
          : text {
            label = "Width of Footing: ";
            key = widthf ;
          }
        }

        : boxed_radio_row {
          label = "Pile Type" ;
          : radio_button {
            key = steeltype ;
            label = "Steel" ;
            mnemonic = "S" ;
          }
          : radio_button {
            key = conctype ;
            label = "Con'c" ;
            mnemonic = "C" ;
          }
        }
        
        
      : row {
          : edit_box {
            label = "Dia";
            key = dia ;
            edit_width = 4 ;            
          }
          : text {
            label = "mm";
            key = diatxt ;
          }
        }

        : row {        
          : edit_box {
            label = "Num";          
            key = num ;
            edit_width = 4 ;            
          }
          : text {
            label = "m"  ;        
            key = numtxt ;
          }
        }
        
        : row {        
          : edit_box {
            label = "Pitch";          
            key = pitch ;
            edit_width = 5 ;            
          }
          : text {
            label = "m";          
            key = pitchtxt ;
          }
        }  

        : row {        
          : edit_box {
            label = "Edge";          
            key = edge ;
            edit_width = 5 ;            
          }
          : text {
            label = "m";
            key = edgetxt ;
          }
        }  

        : row {                
          :button {
            label = "Check";
            key   = check;
            mnemonic = "C";
            width = 11.7;
            fixed_width = true;
          }
        }
        
        : row {        
          : text {
            label = "Result: ";
            key = result ;
          }
        }  
        
      spacer;
      ok_cancel_err;
    }


DLIVE : dialog {
    label = "Dead / Live Anchorage";
    : row {
             height = 3;
            : image_button {
                key = "live";
                width = 6;
//                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "livedead";
                width = 6;
//                fixed_height = 4;
                allow_accept = false;
                color = 5;
             }
            : image_button {
                key = "livelive";
                width = 6;
//                fixed_height = 4;
                allow_accept = false;
                color = 5;
            }

    }
    : edit_box {
      label = "Text";
      key = dltext;
      edit_width = 15;
    }  
    spacer;
    ok_cancel;
} // DLIVE


FANCH : dialog {
    label = "Freyssinet Anchorage";
      : image {
          key = "anchimage";
          width = 10;
          height = 10;
//          aspect_ratio = 0;
          color = 5;
      }
        : boxed_radio_row {
          label = "View" ;
          : radio_button {
            key = "front" ;
            label = "Front" ;
            mnemonic = "F" ;
          }
          : radio_button {
            key = "profile" ;
            label = "Profile" ;
            mnemonic = "P" ;
          }
        }
        : boxed_radio_row {
          label = "Direction" ;
          : radio_button {
            key = "longside" ;
            label = "Longside" ;
//            mnemonic = "F" ;
          }
          : radio_button {
            key = "shortside" ;
            label = "Shortside" ;
//            mnemonic = "P" ;
          }
        }
    : boxed_row {
        label = "Reinforcement";
        :toggle {
          label = "Rebar";
          key = "rebar";
          mnemonic = "R";
        }
        :row {
            : radio_button {
              key = "grid" ;
              label = "Grid" ;
            }
            : radio_button {
              key = "helical" ;
              label = "Helical" ;
            }
        } //row 
    } //row
    : row {
        :toggle {
          label = "Blockout";
          key = "blockout";
//          mnemonic = "J";
        }
    
        :toggle {
          label = "Jack";
          key = "jack";
          mnemonic = "J";
        }
    } //row
    
    : popup_list {
       label = "Anchorage";
       key = alist;
       edit_width = 8;
       list = "3C15\n4C15\n7C15\n9C15\n12C15\n13C15\n19C15\n22C15\n25C15\n25C15P\n27C15\n31C15\n37C15\n55C15\n";
    } //popup_lists

    spacer;
    ok_cancel;
} // FANCH


LSTIFF : dialog {
      label = "Longitudinal Stiffener" ;
      
            : text{
              label = "L=";
              key = length;
            }
      
          : row {      
            : edit_box {
              label = "n";
              key = num ;
              edit_width = 4 ;            
            }
            : text{
              label = "EA";
            }
          }              

          :button {
            label = "Check";
            key   = check;
            mnemonic = "C";
            width = 11.7;
            fixed_width = true;
          }
          :row{
           : column{
             : text {
               label = "result1";
               key = result1 ;
             }
             : text {
             label = "result2";
               key = result2 ;
             }
           }
          }
           :radio_row {
             :radio_button {
               label = "Result-1: ";             
               key = radio1;
             }
             :radio_button {
               label = "Result-2: ";             
               key = radio2;
             }
           }//row
          
          
          : row {
            : edit_box {
              label = "b";
              key = brib ;
            }
            
            : text{
              label = "mm";
            }
          }
          : row {
            : edit_box {
              label = "t";
              key = thick ;
            }
            : text{
              label = "mm";
            }  
          } //row
      

       spacer;
      ok_cancel_err;
    }//dialog


MODELM : dialog {
    label = "Model Markings";
    : row {
             height = 3;
            : image_button {
                key = "roller";
                width = 6;
                allow_accept = true;
                color = 5;
             }
            : image_button {
                key = "hinge";
                width = 6;
                allow_accept = true;
                color = 5;
             }
            : image_button {
                key = "fix";
                width = 6;
                allow_accept = true;
                color = 5;
            }
            : image_button {
                key = "lspring";
                width = 6;
                allow_accept = true;
                color = 5;
            }
            : image_button {
                key = "rspring";
                width = 6;
                allow_accept = true;
                color = 5;
            }

    }
//    : edit_box {
//      label = "Text";
//      key = dltext;
//      edit_width = 15;
//    }  
    spacer;
    ok_cancel;
} // DLIVE


DDRECT : dialog {
    label = "DDRECT";
    : row {
             height = 4;
            : image_button {
                key = "ddrectc";
                width = 5;
                allow_accept = true;
                color = 5;
             }
            : image_button {
                key = "ddrectm";
                width = 5;
                allow_accept = true;
                color = 5;
             }
            : image_button {
                key = "ddrects";
                width = 5;
                allow_accept = true;
                color = 5;
            }

    }
    : row{
    : edit_box {
      label = "T";
      key = ddrect_t;
      edit_width = 15;
    }
    : edit_box {
      label = "L";
      key = ddrect_l;
      edit_width = 15;
    }
    }
    spacer;
    ok_cancel;
} // DLIVE


FCORNER : dialog {
    label = "FCORNER";
    :row{
    : radio_column{

             :radio_button {
               label = "Dia=13  R=140";             
               key = radio13;
             }
             :radio_button {
               label = "Dia=16  R=170";             
               key = radio16;
             }
             :radio_button {
               label = "Dia=19  R=200";             
               key = radio19;
             }
             :radio_button {
               label = "Dia=22  R=240";             
               key = radio22;
             }
             :radio_button {
               label = "Dia=25  R=270";             
               key = radio25;
             }
             :radio_button {
               label = "Dia=29  R=310";             
               key = radio29;
             }
             :radio_button {
               label = "Dia=32  R=340";             
               key = radio32;
             }
    
    }
/*     : column{
    : edit_box {
      edit_width = 4;
      value = "Dia";
    }
    : edit_box {
      edit_width = 4;
      value = "13";
    }
     : edit_box {
      edit_width = 4;
      value = "16";      
    }
    : edit_box {
      edit_width = 4;
      value = "19";      
    }
    : edit_box {
      edit_width = 4;
      value = "22";      
    }
    : edit_box {
      edit_width = 4;
      value = "25";      
    }
    : edit_box {
      edit_width = 4;
      value = "29";      
    }
    : edit_box {
      edit_width = 4;
      value = "32";      
    }
    
    }//column 
    
  : column{
    : edit_box {
      edit_width = 4;
      value = "R";
      alignment = centered;
//      is_enabled = false;
    }
    : edit_box {
      edit_width = 4;
      value = "140";
      alignment = centered;
      }
     : edit_box {
      edit_width = 4;
      value = "170";
      alignment = centered;      
    }
    : edit_box {
      edit_width = 4;
      value = "200";
      alignment = centered;      
    }
    : edit_box {
      edit_width = 4;
      value = "240";
      alignment = centered;      
    }
    : edit_box {
      edit_width = 4;
      value = "270";
      alignment = centered;      
    }
    : edit_box {
      edit_width = 4;
      value = "310";
      alignment = centered;      
    }
    : edit_box {
      edit_width = 4;
      value = "340";
      alignment = centered;      
    }

    }
  : column{
    : edit_box {
      edit_width = 4;
      value = "CL";
    }
    : edit_box {
      edit_width = 4;
      value = "220";
    }
     : edit_box {
      edit_width = 4;
      value = "267";      
    }
    : edit_box {
      edit_width = 4;
      value = "314";      
    }
    : edit_box {
      edit_width = 4;
      value = "377";      
    }
    : edit_box {
      edit_width = 4;
      value = "424";      
    }
    : edit_box {
      edit_width = 4;
      value = "487";      
    }
    : edit_box {
      edit_width = 4;
      value = "534";      
    }

    } */
    
    }
    spacer;
    ok_cancel;
} // DLIVE


CCTC : dialog {
      label = "Change CTC" ;
      
          : text{
              label = "L=";
              key = length;
            }
      
          : row {      
            : edit_box {
//              label = "n";
              key = num ;
              edit_width = 4 ;            
            }
            : text{
              label = "@";
            }
            : edit_box {
//              label = "1";
              key = divl ;
              edit_width = 4 ;            
            }
            : text{
              label = "=";
            }
            
            : text{
              label = "1.250";
              key = ntd;
            }
            
          }              

          : row {      
            : text{
              label = "������  =   ";
            }

            : text{
              label = "50";
              key = re;
            }
            
            : edit_box {
              label = "����������";
              key = rnum ;
              edit_width = 4 ;
              value = "1";
            }
            : text{
              label = "EA";
            }
            

          }

//          :button {
//            label = "Check";
//            key   = check;
//            mnemonic = "C";
//            width = 11.7;
//            fixed_width = true;
//          }

          :list_box {
             label = "Select Case";
             height = 6 ;
             key = lst;
             list = "1\n2\n3\n4";
           }//list box
          
          : row {
            : edit_box {
              label = "Final";
              key = final ;
              edit_width = 40;
            }
          }
          
          : row {
            : text {
              label = "Length=";
              key = flen ;
            }
          }
          
        : boxed_radio_row {
          label = "기존치수선 처리" ;
          : radio_button {
            key = "delold" ;
            label = "지우기" ;
            mnemonic = "X" ;
          }
          : radio_button {
            key = "plus1dan" ;
            label = "+1 Level" ;
            mnemonic = "B" ;
          }
        }//boxed_radio_row

       spacer;
      ok_cancel_err;
    }//dialog


ERA_DLG: dialog {
    label = "Edit Rebar Detail Attribute";
    : column {
      :row{
      : edit_box {
        label = "fck(MPa)";
        key = "fck";
        edit_width = 4;
        alignment = right;
      }
      : edit_box {
        label = "cover(mm)";
        key = "cover";
        edit_width = 4;
        alignment = right;
      }
      }
      :row{
        : edit_box {
          label = "ctc(mm)";
          key = "ctc";
          edit_width = 4;
          alignment = right;
        }
        : edit_box {
          label = "unit L(m)";
          key = "unitl";
          edit_width = 4;
          alignment = right;
        }
      
      }//row
      :row{      
      :toggle {
        label = "Upper";
        key = "up";
        mnemonic = "u";
      }
      : edit_box {
        label = "EQ";
        key = "eq";
        edit_width = 12;
        alignment = right;
      }
      
      }//row
        : edit_box {
          label = "MARK ex:A1-1";
          key = "mark";
          edit_width = 6;
          alignment = right;
        }
      
        : edit_box {
          label = "DIA ex:D29";
          key = "dia";
          edit_width = 6;
          alignment = right;
        }
      
      :row{
        : edit_box {
          label = "VAR1";
          key = "var1l";
          edit_width = 2;
          alignment = right;
        }
        : edit_box {
          label = "=";
          key = "var1v";
          edit_width = 12;
          alignment = right;
        }
      }
      :row {
        : edit_box {
          label = "VAR2";
          key = "var2l";
          edit_width = 2;
          alignment = right;
        }
        : edit_box {
          label = "=";
          key = "var2v";
          edit_width = 12;
          alignment = right;
        }
      }        
      :row {
        : edit_box {
          label = "VAR3";
          key = "var3l";
          edit_width = 2;
          alignment = right;
        }
        : edit_box {
          label = "=";
          key = "var3v";
          edit_width = 12;
          alignment = right;
        }
      }        
      :row {
        : edit_box {
          label = "VAR4";
          key = "var4l";
          edit_width = 2;
          alignment = right;
        }
        : edit_box {
          label = "=";
          key = "var4v";
          edit_width = 12;
          alignment = right;
        }
      }        
      :row {
        : edit_box {
          label = "VAR5";
          key = "var5l";
          edit_width = 2;
          alignment = right;
        }
        : edit_box {
          label = "=";
          key = "var5v";
          edit_width = 12;
          alignment = right;
        }
      }        
      : edit_box {
        label = "NUM ex:50";
        key = "num";
        edit_width = 6;
        alignment = right;
      }
    } //column
    spacer;
    ok_cancel_err;
}//dialog


WCPL : dialog {
      label = "Write Cross Point Length" ;

      : edit_box {
        label = "Text Height" ;
        key = "th" ;
        edit_width = 8 ;
      }
      : edit_box {
        label = "Prefix" ;
        key = "pref" ;
        edit_width = 8 ;
      }
    : popup_list {
       label = "Layer";
       key = laylst;
       edit_width = 8;
       list = "1\n2\n3\n";
    } //popup_lists
      : edit_box {
        label = "Minus Length" ;
        key = "minusl" ;
        edit_width = 8 ;
      }
      : edit_box {
        label = "Cut off Length" ;
        key = "mind" ;
        edit_width = 8 ;
      }

      :toggle {
        label = "Text Angle";
        key = "ta";
        mnemonic = "A";
      }
      
      spacer;
      ok_cancel_err;
    }

WCPL2 : dialog {
      label = "Write Cross Point Length (Line)" ;

      : edit_box {
        label = "Text Height" ;
        key = "wc2th" ;
        edit_width = 8 ;
      }
      : edit_box {
        label = "Prefix" ;
        key = "wc2pref" ;
        edit_width = 8 ;
      }
    : popup_list {
       label = "Layer";
       key = "wc2laylst";
       edit_width = 8;
       list = "1\n2\n3\n";
    } //popup_lists
      : edit_box {
        label = "Delta(mm)" ;
        key = "wc2delta" ;
        edit_width = 8 ;
      }
      : edit_box {
        label = "Cut off Length" ;
        key = "wc2mind" ;
        edit_width = 8 ;
      }

      :toggle {
        label = "Text Angle";
        key = "wc2ta";
        mnemonic = "A";
      }
      
      spacer;
      ok_cancel_err;
    }

DDR : dialog {
      label = "Distance Different Radius" ;

      : row{
      : edit_box {
        label = "Source Radius" ;
        key = "sr" ;
        edit_width = 8 ;
      }
      :button{
       key="slsa";
       label="...";
      }
      }//row
      
      : row{
      : edit_box {
        label = "Target Radius" ;
        key = "tr" ;
        edit_width = 8 ;
      }
      :button{
       key="slta";
       label="...";
      }
      } //row
      
      : edit_box {
        label = "Prefix" ;
        key = "pref" ;
        edit_width = 8 ;
      }
      : edit_box {
        label = "Postfix" ;
        key = "postf" ;
        edit_width = 8 ;
      }

     
      spacer;
      ok_cancel_err;
    }


MEQ : dialog {
      label = "Multi EQualize" ;
        : boxed_radio_row {
          label = "Operation" ;
          : radio_button {
            key = "create" ;
            label = "Create" ;
//            mnemonic = "X" ;
          }
          : radio_button {
            key = "modify" ;
            label = "Modify" ;
//            mnemonic = "B" ;
          }
        }
        : boxed_radio_row {
          label = "Create" ;
          key = "createbox";
          
            : radio_button {
              key = "text" ;
              label = "Text" ;
            }
            : radio_button {
              key = "dimension" ;
              label = "Dimension" ;
            }
            : radio_button {
              key = "txtlist" ;
              label = "Text List" ;
            }
            
//            : radio_button {
//              key = "hordim" ;
//              label = "Hor.Dim" ;
//            }
//            : radio_button {
//              key = "verdim" ;
//              label = "Ver.Dim" ;
//            }
//            : radio_button {
//              key = "alidim" ;
//              label = "Ali.Dim" ;
//            }
                    
        }

      :toggle {
        label = "Change Value";
        key = "chval";
      }
      
      :boxed_column{
        key="chvalbox";
        : edit_box {
          label = "Prefix" ;
          key = "pref" ;
          edit_width = 8 ;
        }
        : edit_box {
          label = "Postfix" ;
          key = "postf" ;
          edit_width = 8 ;
        }
        : edit_box {
          label = "Factor" ;
          key = "factor" ;
          edit_width = 8 ;
        }
        : edit_box {
          label = "Delta(mm)" ;
          key = "delta" ;
          edit_width = 8 ;
        }
        :toggle {
          label = "Original Distance";
          key = "oridist";
        }

      }//boxed_row
      :toggle {
        label = "Gol";
        key = "gol";
      }
     
      spacer;
      ok_cancel_err;
    }


MTRIMB : dialog {
      label = "Multi Trim between" ;

      : edit_box {
        label = "Text Height" ;
        key = "mtrth" ;
        edit_width = 8 ;
      }
      : edit_box {
        label = "Prefix" ;
        key = "mtrpref" ;
        edit_width = 8 ;
      }
    : popup_list {
       label = "V-stiff Layer";
       key = "mtrvlaylst";
       edit_width = 8;
       list = "1\n2\n3\n";
    } //popup_lists
    : popup_list {
       label = "H-stiff Layer";
       key = "mtrhlaylst";
       edit_width = 8;
       list = "1\n2\n3\n";
    } //popup_lists

    : edit_box {
        label = "Gap(mm)" ;
        key = "mtrgap" ;
        edit_width = 8 ;
      }

      :toggle {
        label = "Write Length";
        key = "mtrwlen";
      }
      
      spacer;
      ok_cancel_err;
    }
    