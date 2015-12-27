<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" omit-xml-declaration="yes" indent="no" media-type="text/plain" />
<xsl:template match="/" xml:space="default">
<xsl:for-each select="current_observation">
<xsl:value-of select="location/latitude"/><xsl:text>|</xsl:text> 
<xsl:value-of select="location/longitude"/><xsl:text>|</xsl:text> 
<xsl:value-of select="station_id"/><xsl:text>|</xsl:text> 
<xsl:value-of select="location/elevation"/><xsl:text>|</xsl:text> 
<xsl:value-of select="temp_f"/><xsl:text>|</xsl:text>
<xsl:value-of select="dewpoint_f"/><xsl:text>|</xsl:text>
<xsl:value-of select="relative_humidity"/><xsl:text>|</xsl:text> 
<xsl:value-of select="wind_dir"/><xsl:text>|</xsl:text>
<xsl:value-of select="wind_mph"/><xsl:text>|</xsl:text>
<xsl:value-of select="pressure_in"/><xsl:text>|</xsl:text> 
<xsl:value-of select="precip_today_in"/><xsl:text>|</xsl:text> 
<xsl:value-of select="observation_time"/><xsl:text>|</xsl:text>
<xsl:value-of select="location/neighborhood"/><xsl:text>|</xsl:text> 
<xsl:value-of select="location/city"/><xsl:text>|</xsl:text> 
<xsl:value-of select="location/state"/>
</xsl:for-each>

</xsl:template>
</xsl:stylesheet>
