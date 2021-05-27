# xmlfuzzer
XMLFuzzer based off http://komar.in/en/code/xmlfuzzer

I do NOT support this application. I am just trying to keep this somewhat alive considering that many of such applications are dead.
The site which supports this seems dead and the application doesn't work as shown in the website. 


### Install / Compile 
Anywas as far as I can tell the application runs in linux and it looks like these were used to compile it . There is a release in the xmlfuzzer-bin-x86-0.04 folder. 
Any assistence to make this cross platform would be strongly apprecated. 

```
OCamlduce compiler >= 3.11;
GNU Make;
xml-light;
extlib for OCaml.
```

### Usage

From the application directory you can run the application like so :
` ./xmlfuzzer -xsd OfficeOpenXML-XMLSchema/wml.xsd -root-elem document -max-elem 1 `


Here is output from the website : the command and output seems to be different than what I receive , but the program appears to work. 

```
./xmlfuzzer -xsd ../ooxml/OfficeOpenXML-XMLSchema/wml.xsd -root-elem document -max-elem 10 2> /dev/null | tidy -utf8 -xml -i 2> /dev/null
<w:document xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:body>
    <w:sdt>
      <w:sdtContent>
        <w:sdt>
          <w:sdtPr />
        </w:sdt>
      </w:sdtContent>
    </w:sdt>
    <w:sectPr w:rsidDel="Be0N" w:rsidRPr="-4R^">
      <w:footerReference w:type="default" r:id="" />
      <w:footnotePr>
        <w:pos w:val="pageBottom" />
        <w:numStart w:val="1584397285253833485" />
        <w:numRestart w:val="continuous" />
      </w:footnotePr>
      <w:endnotePr>
        <w:numStart w:val="-4241193316720752020" />
      </w:endnotePr>
    </w:sectPr>
  </w:body>
</w:document>

```
