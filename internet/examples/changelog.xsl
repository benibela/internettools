<?xml version="1.0" encoding="UTF-8"?>
<!-- 
Einbinden mit <?xml-stylesheet type="text/xsl" href="changelog.xsl"?>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>
  <xsl:output encoding="UTF-8"/>

  <xsl:template match="/">
    <html>
      <head><title><xsl:value-of select="/changelog/@program"/> Changelog</title></head>
            <body onload="document.links[0].focus()" style="background-color:#FFFFCC">
              <a name="top" href="#top"></a>
              <h1><xsl:value-of select="/changelog/@program"/></h1>
              <xsl:for-each select="changelog/build">
               <xsl:if test="not(@status='debug')">
                 <h2 style="margin-left:0.3em">Version <xsl:value-of select="substring(@version,1,1)"/>,<xsl:value-of select="substring(@version,2,4)"/>  <span style="font-size:small"> vom <xsl:value-of select="@date"/></span></h2> 
                 <div style="margin-left:2em">
                 <xsl:if test="string-length(change) > 0">
                 Änderungen:
                 <ul>
                 <xsl:for-each select="change">
                 <li><xsl:value-of select="."/></li>
                 </xsl:for-each></ul>
                 </xsl:if>
                 <xsl:if test="string-length(add) > 0">
                 Hinzufügungen:
                 <ul>
                 <xsl:for-each select="add">
                 <li><xsl:value-of select="."/></li>
                 </xsl:for-each>
                 </ul>
                 </xsl:if>
                 <xsl:if test="string-length(fix) > 0">
                 Beseitigte Fehler:
                 <ul>
                 <xsl:for-each select="fix">
                 <li><xsl:if test="@level = 'critical'"><xsl:attribute name="style">color: red; font-weight:bold;</xsl:attribute></xsl:if>
                     <xsl:if test="@level = 'major'"><xsl:attribute name="style">font-weight:bold;</xsl:attribute></xsl:if>
                     <xsl:value-of select="."/></li>
                 </xsl:for-each></ul>
                 </xsl:if>
                 </div>
               </xsl:if>
              </xsl:for-each>
            </body>
       </html>
  </xsl:template>

</xsl:stylesheet>
