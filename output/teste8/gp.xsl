<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform' version='1.0'>
  <xsl:template match='/'>
    <html>
      <head><title>GP</title>
      <style>
	table1 {
	display: table;
	border-collapse: separate;
	border-spacing: 2px;
	border-color: gray;
	}
	th, td {
	padding: 5px;
	}
      </style>
      </head>
      <body>
	<h1>genetic programming</h1>
	<table id='table1'>
	  <tr>
	    <td>
	      <li>
		Max população: <xsl:value-of select='gp/max-populacao'/>
	      </li>
	    </td>
	    <td>
	      <li>
		Taxa mutação: <xsl:value-of select='gp/taxa-mutacao'/>
	      </li>
	    </td>
	  </tr>
	  <tr>
	    <td>
	      <li>
		Chance mutação: <xsl:value-of select='gp/chance-mutacao'/>
	      </li>
	    </td>
	    <td>
	      <li>
		Max depth: <xsl:value-of select='gp/max-depth'/>
	      </li>
	    </td>
	  </tr>
	</table>
	<h3>
	  Formula encontrada: <xsl:value-of select='gp/formula'/> - Fitness: <xsl:value-of select='gp/fitness'/>
	</h3>
	<h2>Dados entrada</h2>
	<table border='1'>
	  <tr>
	    <th>
	      Entrada
	    </th>
	    <th>
	      Saida
	    </th>
	  </tr>
	  <xsl:for-each select='gp/tabela/valores'>
	    <tr>
	      <td>
		<xsl:value-of select='input'/>
	      </td>
	      <td>
		<xsl:value-of select='output'/>
	      </td>
	    </tr>
	  </xsl:for-each>
	</table>
	<br></br>
	<br></br>
	<img src= '{/gp/grafico/node()}' />
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>

