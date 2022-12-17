# build_fixed_param_modal_window

    Code
      build_fixed_param_modal_window(mod_6par)
    Output
      [1] "<div class=\"user_input_popup\">\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"CL1_value-label\" for=\"CL1_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">CL1 Value</span>\n  </label>\n  <input id=\"CL1_value\" type=\"text\" class=\"form-control\" value=\"1\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"V1_value-label\" for=\"V1_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">V1 Value</span>\n  </label>\n  <input id=\"V1_value\" type=\"text\" class=\"form-control\" value=\"30\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"KA1_value-label\" for=\"KA1_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">KA1 Value</span>\n  </label>\n  <input id=\"KA1_value\" type=\"text\" class=\"form-control\" value=\"1.3\"/>\n</div></div>\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"CL1_units-label\" for=\"CL1_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">CL1 Units</span>\n  </label>\n  <input id=\"CL1_units\" type=\"text\" class=\"form-control\" value=\".\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"V1_units-label\" for=\"V1_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">V1 Units</span>\n  </label>\n  <input id=\"V1_units\" type=\"text\" class=\"form-control\" value=\".\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"KA1_units-label\" for=\"KA1_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">KA1 Units</span>\n  </label>\n  <input id=\"KA1_units\" type=\"text\" class=\"form-control\" value=\".\"/>\n</div></div>\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"CL2_value-label\" for=\"CL2_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">CL2 Value</span>\n  </label>\n  <input id=\"CL2_value\" type=\"text\" class=\"form-control\" value=\"1\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"V2_value-label\" for=\"V2_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">V2 Value</span>\n  </label>\n  <input id=\"V2_value\" type=\"text\" class=\"form-control\" value=\"30\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"KA2_value-label\" for=\"KA2_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">KA2 Value</span>\n  </label>\n  <input id=\"KA2_value\" type=\"text\" class=\"form-control\" value=\"1.3\"/>\n</div></div>\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"CL2_units-label\" for=\"CL2_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">CL2 Units</span>\n  </label>\n  <input id=\"CL2_units\" type=\"text\" class=\"form-control\" value=\".\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"V2_units-label\" for=\"V2_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">V2 Units</span>\n  </label>\n  <input id=\"V2_units\" type=\"text\" class=\"form-control\" value=\".\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"KA2_units-label\" for=\"KA2_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">KA2 Units</span>\n  </label>\n  <input id=\"KA2_units\" type=\"text\" class=\"form-control\" value=\".\"/>\n</div></div>\n</div>"

---

    Code
      build_fixed_param_modal_window(def_mod)
    Output
      [1] "<div class=\"user_input_popup\">\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVCL_value-label\" for=\"TVCL_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVCL Value</span>\n  </label>\n  <input id=\"TVCL_value\" type=\"text\" class=\"form-control\" value=\"0.25\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVV2_value-label\" for=\"TVV2_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVV2 Value</span>\n  </label>\n  <input id=\"TVV2_value\" type=\"text\" class=\"form-control\" value=\"20\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVQ_value-label\" for=\"TVQ_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVQ Value</span>\n  </label>\n  <input id=\"TVQ_value\" type=\"text\" class=\"form-control\" value=\"1\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVV3_value-label\" for=\"TVV3_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVV3 Value</span>\n  </label>\n  <input id=\"TVV3_value\" type=\"text\" class=\"form-control\" value=\"30\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVKA_value-label\" for=\"TVKA_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVKA Value</span>\n  </label>\n  <input id=\"TVKA_value\" type=\"text\" class=\"form-control\" value=\"0.5\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVF1_value-label\" for=\"TVF1_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVF1 Value</span>\n  </label>\n  <input id=\"TVF1_value\" type=\"text\" class=\"form-control\" value=\"0.75\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVVMAX_value-label\" for=\"TVVMAX_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVVMAX Value</span>\n  </label>\n  <input id=\"TVVMAX_value\" type=\"text\" class=\"form-control\" value=\"5\"/>\n</div></div>\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVCL_units-label\" for=\"TVCL_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVCL Units</span>\n  </label>\n  <input id=\"TVCL_units\" type=\"text\" class=\"form-control\" value=\"L/day\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVV2_units-label\" for=\"TVV2_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVV2 Units</span>\n  </label>\n  <input id=\"TVV2_units\" type=\"text\" class=\"form-control\" value=\"L\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVQ_units-label\" for=\"TVQ_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVQ Units</span>\n  </label>\n  <input id=\"TVQ_units\" type=\"text\" class=\"form-control\" value=\"L/day\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVV3_units-label\" for=\"TVV3_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVV3 Units</span>\n  </label>\n  <input id=\"TVV3_units\" type=\"text\" class=\"form-control\" value=\"L\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVKA_units-label\" for=\"TVKA_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVKA Units</span>\n  </label>\n  <input id=\"TVKA_units\" type=\"text\" class=\"form-control\" value=\"1/day\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVF1_units-label\" for=\"TVF1_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVF1 Units</span>\n  </label>\n  <input id=\"TVF1_units\" type=\"text\" class=\"form-control\" value=\"unitless\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVVMAX_units-label\" for=\"TVVMAX_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVVMAX Units</span>\n  </label>\n  <input id=\"TVVMAX_units\" type=\"text\" class=\"form-control\" value=\"mg/day\"/>\n</div></div>\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVKM_value-label\" for=\"TVKM_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVKM Value</span>\n  </label>\n  <input id=\"TVKM_value\" type=\"text\" class=\"form-control\" value=\"1\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"DOSE_value-label\" for=\"DOSE_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">DOSE Value</span>\n  </label>\n  <input id=\"DOSE_value\" type=\"text\" class=\"form-control\" value=\"0\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WT_value-label\" for=\"WT_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WT Value</span>\n  </label>\n  <input id=\"WT_value\" type=\"text\" class=\"form-control\" value=\"70\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WTREF_value-label\" for=\"WTREF_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WTREF Value</span>\n  </label>\n  <input id=\"WTREF_value\" type=\"text\" class=\"form-control\" value=\"70\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WTCL_value-label\" for=\"WTCL_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WTCL Value</span>\n  </label>\n  <input id=\"WTCL_value\" type=\"text\" class=\"form-control\" value=\"0.75\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WTV2_value-label\" for=\"WTV2_value\">\n    <span style=\"color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WTV2 Value</span>\n  </label>\n  <input id=\"WTV2_value\" type=\"text\" class=\"form-control\" value=\"1\"/>\n</div></div>\n  <div class=\"left_align_col\"><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"TVKM_units-label\" for=\"TVKM_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">TVKM Units</span>\n  </label>\n  <input id=\"TVKM_units\" type=\"text\" class=\"form-control\" value=\"mg/L\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"DOSE_units-label\" for=\"DOSE_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">DOSE Units</span>\n  </label>\n  <input id=\"DOSE_units\" type=\"text\" class=\"form-control\" value=\"mg\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WT_units-label\" for=\"WT_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WT Units</span>\n  </label>\n  <input id=\"WT_units\" type=\"text\" class=\"form-control\" value=\"kg\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WTREF_units-label\" for=\"WTREF_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WTREF Units</span>\n  </label>\n  <input id=\"WTREF_units\" type=\"text\" class=\"form-control\" value=\"kg\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WTCL_units-label\" for=\"WTCL_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WTCL Units</span>\n  </label>\n  <input id=\"WTCL_units\" type=\"text\" class=\"form-control\" value=\"unitless\"/>\n</div><div class=\"form-group shiny-input-container\">\n  <label class=\"control-label\" id=\"WTV2_units-label\" for=\"WTV2_units\">\n    <span style=\"color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;\">WTV2 Units</span>\n  </label>\n  <input id=\"WTV2_units\" type=\"text\" class=\"form-control\" value=\"unitless\"/>\n</div></div>\n</div>"

# reorder_modal_popup_elements

    Code
      reorder_modal_popup_elements(mod_6par_modal)
    Output
      <div class="user_input_popup">
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="CL1_value-label" for="CL1_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">CL1 Value</span>
        </label>
        <input id="CL1_value" type="text" class="form-control" value="1"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="V1_value-label" for="V1_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">V1 Value</span>
        </label>
        <input id="V1_value" type="text" class="form-control" value="30"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="KA1_value-label" for="KA1_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">KA1 Value</span>
        </label>
        <input id="KA1_value" type="text" class="form-control" value="1.3"/>
      </div></div>
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="CL1_units-label" for="CL1_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">CL1 Units</span>
        </label>
        <input id="CL1_units" type="text" class="form-control" value="."/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="V1_units-label" for="V1_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">V1 Units</span>
        </label>
        <input id="V1_units" type="text" class="form-control" value="."/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="KA1_units-label" for="KA1_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">KA1 Units</span>
        </label>
        <input id="KA1_units" type="text" class="form-control" value="."/>
      </div></div>
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="CL2_value-label" for="CL2_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">CL2 Value</span>
        </label>
        <input id="CL2_value" type="text" class="form-control" value="1"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="V2_value-label" for="V2_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">V2 Value</span>
        </label>
        <input id="V2_value" type="text" class="form-control" value="30"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="KA2_value-label" for="KA2_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">KA2 Value</span>
        </label>
        <input id="KA2_value" type="text" class="form-control" value="1.3"/>
      </div></div>
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="CL2_units-label" for="CL2_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">CL2 Units</span>
        </label>
        <input id="CL2_units" type="text" class="form-control" value="."/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="V2_units-label" for="V2_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">V2 Units</span>
        </label>
        <input id="V2_units" type="text" class="form-control" value="."/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="KA2_units-label" for="KA2_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">KA2 Units</span>
        </label>
        <input id="KA2_units" type="text" class="form-control" value="."/>
      </div></div>
      </div>

---

    Code
      reorder_modal_popup_elements(def_mod_modal)
    Output
      <div class="user_input_popup">
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="TVCL_value-label" for="TVCL_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVCL Value</span>
        </label>
        <input id="TVCL_value" type="text" class="form-control" value="0.25"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVV2_value-label" for="TVV2_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVV2 Value</span>
        </label>
        <input id="TVV2_value" type="text" class="form-control" value="20"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVQ_value-label" for="TVQ_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVQ Value</span>
        </label>
        <input id="TVQ_value" type="text" class="form-control" value="1"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVV3_value-label" for="TVV3_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVV3 Value</span>
        </label>
        <input id="TVV3_value" type="text" class="form-control" value="30"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVKA_value-label" for="TVKA_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVKA Value</span>
        </label>
        <input id="TVKA_value" type="text" class="form-control" value="0.5"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVF1_value-label" for="TVF1_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVF1 Value</span>
        </label>
        <input id="TVF1_value" type="text" class="form-control" value="0.75"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVVMAX_value-label" for="TVVMAX_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVVMAX Value</span>
        </label>
        <input id="TVVMAX_value" type="text" class="form-control" value="5"/>
      </div></div>
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="TVCL_units-label" for="TVCL_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVCL Units</span>
        </label>
        <input id="TVCL_units" type="text" class="form-control" value="L/day"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVV2_units-label" for="TVV2_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVV2 Units</span>
        </label>
        <input id="TVV2_units" type="text" class="form-control" value="L"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVQ_units-label" for="TVQ_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVQ Units</span>
        </label>
        <input id="TVQ_units" type="text" class="form-control" value="L/day"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVV3_units-label" for="TVV3_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVV3 Units</span>
        </label>
        <input id="TVV3_units" type="text" class="form-control" value="L"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVKA_units-label" for="TVKA_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVKA Units</span>
        </label>
        <input id="TVKA_units" type="text" class="form-control" value="1/day"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVF1_units-label" for="TVF1_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVF1 Units</span>
        </label>
        <input id="TVF1_units" type="text" class="form-control" value="unitless"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="TVVMAX_units-label" for="TVVMAX_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVVMAX Units</span>
        </label>
        <input id="TVVMAX_units" type="text" class="form-control" value="mg/day"/>
      </div></div>
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="TVKM_value-label" for="TVKM_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVKM Value</span>
        </label>
        <input id="TVKM_value" type="text" class="form-control" value="1"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="DOSE_value-label" for="DOSE_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">DOSE Value</span>
        </label>
        <input id="DOSE_value" type="text" class="form-control" value="0"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WT_value-label" for="WT_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WT Value</span>
        </label>
        <input id="WT_value" type="text" class="form-control" value="70"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WTREF_value-label" for="WTREF_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WTREF Value</span>
        </label>
        <input id="WTREF_value" type="text" class="form-control" value="70"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WTCL_value-label" for="WTCL_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WTCL Value</span>
        </label>
        <input id="WTCL_value" type="text" class="form-control" value="0.75"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WTV2_value-label" for="WTV2_value">
          <span style="color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WTV2 Value</span>
        </label>
        <input id="WTV2_value" type="text" class="form-control" value="1"/>
      </div></div>
        <div class="left_align_col"><div class="form-group shiny-input-container">
        <label class="control-label" id="TVKM_units-label" for="TVKM_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">TVKM Units</span>
        </label>
        <input id="TVKM_units" type="text" class="form-control" value="mg/L"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="DOSE_units-label" for="DOSE_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">DOSE Units</span>
        </label>
        <input id="DOSE_units" type="text" class="form-control" value="mg"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WT_units-label" for="WT_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WT Units</span>
        </label>
        <input id="WT_units" type="text" class="form-control" value="kg"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WTREF_units-label" for="WTREF_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WTREF Units</span>
        </label>
        <input id="WTREF_units" type="text" class="form-control" value="kg"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WTCL_units-label" for="WTCL_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WTCL Units</span>
        </label>
        <input id="WTCL_units" type="text" class="form-control" value="unitless"/>
      </div><div class="form-group shiny-input-container">
        <label class="control-label" id="WTV2_units-label" for="WTV2_units">
          <span style="color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;">WTV2 Units</span>
        </label>
        <input id="WTV2_units" type="text" class="form-control" value="unitless"/>
      </div></div>
      </div>

# reorder_modal_popup_children

    Code
      reorder_modal_popup_children(test_modal)
    Output
      <div class="modal sbs-modal fade" id="rx_docs_modal_window" tabindex="-1" data-sbs-trigger="rx_docs_modal_button">
        <div class="modal-dialog modal-lg">
          <div class="modal-content">
            <div class="modal-header">
              <h4 class="modal-title">Rx Dosing Docs</h4>
              <button type="button" class="close" data-dismiss="modal">
                <span></span>
              </button>
            </div>
            <div class="modal-body">
              <div class="row">
                <div class="col-sm-12">
                  <div class="user_input_popup"><p><head>
      <link rel="stylesheet" type="text/css" href="style.css">
      </head></p>
      
      <h1>Create intervention objects from Rx input</h1>
      
      <p>Source: <code>https://mrgsolve.org/docs/reference/ev_rx.html</code></p>
      
      <p>See details below for Rx specification. Actual parsing is done by
      parse_rx; this function can be used to debug Rx inputs.</p>
      
      <pre><code class="r, eval=FALSE">mrgsolve::ev_rx(x, y, ...)
      
      # S4 method for mrgmod,character
      mrgsolve::ev_rx(x, y, ...)
      
      # S4 method for character,missing
      mrgsolve::ev_rx(x, df = FALSE, ...)
      
      mrgsolve::parse_rx(x)
      </code></pre>
      
      <!-- ::: -->
      
      <!-- ::: -->
      
      <!-- ::: {#arguments} -->
      
      <h2>Arguments</h2>
      
      <dl>
        <dt><strong>x</strong></dt>
        <dd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; a model object or character Rx input</dd>
        <dt><strong>y</strong></dt>
        <dd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; character Rx input; see details</dd>
        <dt><strong>&hellip;</strong></dt>
        <dd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; not used at this time</dd>
        <dt><strong>df</strong></dt>
        <dd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; if TRUE then a data frame is returned</dd>
      </dl>
      
      <!-- x <br> &ndash;>
      
      <!-- : &nbsp;&nbsp;&nbsp;&nbsp; a model object or `character` Rx input -->
      
      <!-- y -->
      
      <!-- : \ \ \ \ \ \ `character` Rx input; see details -->
      
      <!-- \... -->
      
      <!-- : \ \ \ \ \ \ not used at this time -->
      
      <!-- df -->
      
      <!-- : \ \ \ \ \ \ if `TRUE` then a data frame is returned -->
      
      <h2>Value</h2>
      
      <p>The method dispatched on model object (<code>mrgmod</code>) returns another model
      object. The <code>character</code> method returns an event object. The <code>parse_rx</code>
      function return a list named with arguments for the event object
      constructor <code>ev</code>.</p>
      
      <h2>Rx specification</h2>
      
      <ul>
      <li><p>The dose is found at the start of the string by sequential digits;
      this may be integer, decimal, or in scientific notation</p></li>
      <li><p>Use <code>in</code> to identify the dosing compartment number; must be integer</p></li>
      <li><p>Use <code>q</code> to identify the dosing interval; must be integer or decimal
      number (but not scientific notation)</p></li>
      <li><p>Use <code>over</code> to indicate an infusion and its duration; integer or
      decimal number</p></li>
      <li><p>Use <code>x</code> to indicate total number of doses; must be integer</p></li>
      <li><p>Use <code>then</code> or <code>,</code> to separate dosing periods</p></li>
      <li><p>User <code>after</code> to insert a lag in the start of a period; integer or
      decimal number (but not scientific notation)</p></li>
      </ul>
      
      <h2>Examples</h2>
      
      <pre><code class="r, eval=FALSE"># example(&quot;ev_rx&quot;)
      
      mrgsolve::ev_rx(&quot;100&quot;)
      #&gt; Events:
      #&gt;   time amt cmt evid
      #&gt; 1    0 100   1    1
      
      mrgsolve::ev_rx(&quot;100 in 2&quot;)
      #&gt; Events:
      #&gt;   time amt cmt evid
      #&gt; 1    0 100   2    1
      
      mrgsolve::ev_rx(&quot;100 q12 x 3&quot;)
      #&gt; Events:
      #&gt;   time amt ii addl cmt evid
      #&gt; 1    0 100 12    2   1    1
      
      mrgsolve::ev_rx(&quot;100 over 2&quot;)
      #&gt; Events:
      #&gt;   time amt rate cmt evid
      #&gt; 1    0 100   50   1    1
      
      mrgsolve::ev_rx(&quot;100 q 24 x 3 then 50 q12 x 2&quot;)
      #&gt; Events:
      #&gt;   time amt ii addl cmt evid
      #&gt; 1    0 100 24    2   1    1
      #&gt; 2   72  50 12    1   1    1
      
      mrgsolve::ev_rx(&quot;100 then 50 q 24 after 12&quot;)
      #&gt; Events:
      #&gt;   time amt ii addl cmt evid
      #&gt; 1    0 100  0    0   1    1
      #&gt; 2   12  50 24    0   1    1
      
      mrgsolve::ev_rx(&quot;100.2E-2 q4&quot;)
      #&gt; Events:
      #&gt;   time   amt ii cmt evid
      #&gt; 1    0 1.002  4   1    1
      
      mrgsolve::ev_rx(&quot;100 over 2.23&quot;)
      #&gt; Events:
      #&gt;   time amt     rate cmt evid
      #&gt; 1    0 100 44.84305   1    1
      
      mrgsolve::ev_rx(&quot;100 q 12 x 3&quot;)
      #&gt; Events:
      #&gt;   time amt ii addl cmt evid
      #&gt; 1    0 100 12    2   1    1
      
      mrgsolve::parse_rx(&quot;100 mg q 24 then 200 mg q12&quot;)
      #&gt; [[1]]
      #&gt; [[1]]$time
      #&gt; [1] 0
      #&gt; 
      #&gt; [[1]]$cmt
      #&gt; [1] 1
      #&gt; 
      #&gt; [[1]]$amt
      #&gt; [1] 100
      #&gt; 
      #&gt; [[1]]$ii
      #&gt; [1] 24
      #&gt; 
      #&gt; 
      #&gt; [[2]]
      #&gt; [[2]]$time
      #&gt; [1] 0
      #&gt; 
      #&gt; [[2]]$cmt
      #&gt; [1] 1
      #&gt; 
      #&gt; [[2]]$amt
      #&gt; [1] 200
      #&gt; 
      #&gt; [[2]]$ii
      #&gt; [1] 12
      #&gt; 
      #&gt; 
      </code></pre>
      
      <p>Developed by Kyle T Baron.</p>
      
      <p>Site built with <code>pkgdown</code>
      2.0.1.</p>
      </div>
                </div>
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
            </div>
          </div>
        </div>
      </div>

