package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.Writer;
import javax.imageio.metadata.IIOMetadataNode;
import org.apache.tools.ant.util.DOMElementWriter.XmlNamespacePolicy;
import org.junit.Test;
import org.w3c.dom.Element;

public class DOMElementWriterDiffblueTest {
  /**
   * Test {@link DOMElementWriter#DOMElementWriter()}.
   * <p>
   * Method under test: {@link DOMElementWriter#DOMElementWriter()}
   */
  @Test
  public void testNewDOMElementWriter() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"gt", "amp", "lt", "apos", "quot"}, (new DOMElementWriter()).knownEntities);
  }

  /**
   * Test {@link DOMElementWriter#DOMElementWriter(boolean)}.
   * <p>
   * Method under test: {@link DOMElementWriter#DOMElementWriter(boolean)}
   */
  @Test
  public void testNewDOMElementWriter2() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"gt", "amp", "lt", "apos", "quot"}, (new DOMElementWriter(true)).knownEntities);
  }

  /**
   * Test {@link DOMElementWriter#DOMElementWriter(boolean, XmlNamespacePolicy)}.
   * <p>
   * Method under test: {@link DOMElementWriter#DOMElementWriter(boolean, XmlNamespacePolicy)}
   */
  @Test
  public void testNewDOMElementWriter3() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"gt", "amp", "lt", "apos", "quot"},
        (new DOMElementWriter(true, new XmlNamespacePolicy(true, true))).knownEntities);
  }

  /**
   * Test {@link DOMElementWriter#write(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, Writer, int, String)}
   */
  @Test
  public void testWriteWithElementOutIndentIndentWith() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.write(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo xmlns=\"\" />\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, Writer, int, String)}
   */
  @Test
  public void testWriteWithElementOutIndentIndentWith2() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();

    IIOMetadataNode element = new IIOMetadataNode("foo");
    element.appendChild(new IIOMetadataNode("<"));
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.write(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo>\nIndent WithIndent With<< />\nIndent With</foo>\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, Writer, int, String)}
   */
  @Test
  public void testWriteWithElementOutIndentIndentWith3() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();

    IIOMetadataNode element = new IIOMetadataNode("foo");
    element.appendChild(new IIOMetadataNode("<"));
    element.appendChild(new IIOMetadataNode("<"));
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.write(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo>\nIndent WithIndent With<< />\nIndent WithIndent With<< />\nIndent With</foo>\n",
        out.toString());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, Writer, int, String)}
   */
  @Test
  public void testWriteWithElementOutIndentIndentWith4() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));

    IIOMetadataNode element = new IIOMetadataNode("foo");
    element.appendChild(new IIOMetadataNode("<"));
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.write(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo xmlns=\"\">\nIndent WithIndent With<< />\nIndent With</foo>\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <ul>
   *   <li>Then {@link StringWriter#StringWriter()} toString is {@code Indent With<foo />}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, Writer, int, String)}
   */
  @Test
  public void testWriteWithElementOutIndentIndentWith_thenStringWriterToStringIsIndentWithFoo() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.write(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo />\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, OutputStream)} with {@code root}, {@code out}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, OutputStream)}
   */
  @Test
  public void testWriteWithRootOut() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    IIOMetadataNode root = new IIOMetadataNode("foo");
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    domElementWriter.write(root, out);

    // Assert
    byte[] expectedToByteArrayResult = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<foo />\n".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, out.toByteArray());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, OutputStream)} with {@code root}, {@code out}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, OutputStream)}
   */
  @Test
  public void testWriteWithRootOut2() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));
    IIOMetadataNode root = new IIOMetadataNode("foo");
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    domElementWriter.write(root, out);

    // Assert
    byte[] expectedToByteArrayResult = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<foo xmlns=\"\" />\n"
        .getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, out.toByteArray());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, OutputStream)} with {@code root}, {@code out}.
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, OutputStream)}
   */
  @Test
  public void testWriteWithRootOut3() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(false, new XmlNamespacePolicy(true, true));
    IIOMetadataNode root = new IIOMetadataNode("foo");
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    domElementWriter.write(root, out);

    // Assert
    byte[] expectedToByteArrayResult = "<foo xmlns=\"\" />\n".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, out.toByteArray());
  }

  /**
   * Test {@link DOMElementWriter#write(Element, OutputStream)} with {@code root}, {@code out}.
   * <ul>
   *   <li>Then array length is ninety-eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, OutputStream)}
   */
  @Test
  public void testWriteWithRootOut_thenArrayLengthIsNinetyEight() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();

    IIOMetadataNode root = new IIOMetadataNode("foo");
    root.appendChild(new IIOMetadataNode("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"));
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    domElementWriter.write(root, out);

    // Assert
    assertEquals(98, out.toByteArray().length);
  }

  /**
   * Test {@link DOMElementWriter#write(Element, OutputStream)} with {@code root}, {@code out}.
   * <ul>
   *   <li>Then array length is one hundred seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#write(Element, OutputStream)}
   */
  @Test
  public void testWriteWithRootOut_thenArrayLengthIsOneHundredSeven() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));

    IIOMetadataNode root = new IIOMetadataNode("foo");
    root.appendChild(new IIOMetadataNode("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"));
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    domElementWriter.write(root, out);

    // Assert
    assertEquals(107, out.toByteArray().length);
  }

  /**
   * Test {@link DOMElementWriter#writeXMLDeclaration(Writer)}.
   * <ul>
   *   <li>Then {@link StringWriter#StringWriter()} toString is {@code <?xml version="1.0" encoding="UTF-8"?>}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#writeXMLDeclaration(Writer)}
   */
  @Test
  public void testWriteXMLDeclaration_thenStringWriterToStringIsXmlVersion10EncodingUtf8() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    StringWriter wri = new StringWriter();

    // Act
    domElementWriter.writeXMLDeclaration(wri);

    // Assert
    assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", wri.toString());
  }

  /**
   * Test {@link DOMElementWriter#openElement(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <p>
   * Method under test: {@link DOMElementWriter#openElement(Element, Writer, int, String)}
   */
  @Test
  public void testOpenElementWithElementOutIndentIndentWith() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.openElement(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo>", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#openElement(Element, Writer, int, String)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}.
   * <p>
   * Method under test: {@link DOMElementWriter#openElement(Element, Writer, int, String)}
   */
  @Test
  public void testOpenElementWithElementOutIndentIndentWith2() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.openElement(element, out, 1, "Indent With");

    // Assert
    assertEquals("Indent With<foo xmlns=\"\">", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}, {@code hasChildren}.
   * <p>
   * Method under test: {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)}
   */
  @Test
  public void testOpenElementWithElementOutIndentIndentWithHasChildren() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.openElement(element, out, 1, "Indent With", true);

    // Assert
    assertEquals("Indent With<foo>", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}, {@code hasChildren}.
   * <p>
   * Method under test: {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)}
   */
  @Test
  public void testOpenElementWithElementOutIndentIndentWithHasChildren2() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.openElement(element, out, 1, "Indent With", true);

    // Assert
    assertEquals("Indent With<foo xmlns=\"\">", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}, {@code hasChildren}.
   * <p>
   * Method under test: {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)}
   */
  @Test
  public void testOpenElementWithElementOutIndentIndentWithHasChildren3() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.openElement(element, out, 1, "Indent With", false);

    // Assert
    assertEquals("Indent With<foo />\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)} with {@code element}, {@code out}, {@code indent}, {@code indentWith}, {@code hasChildren}.
   * <p>
   * Method under test: {@link DOMElementWriter#openElement(Element, Writer, int, String, boolean)}
   */
  @Test
  public void testOpenElementWithElementOutIndentIndentWithHasChildren4() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.openElement(element, out, 1, "Indent With", false);

    // Assert
    assertEquals("Indent With<foo xmlns=\"\" />\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#closeElement(Element, Writer, int, String, boolean)}.
   * <p>
   * Method under test: {@link DOMElementWriter#closeElement(Element, Writer, int, String, boolean)}
   */
  @Test
  public void testCloseElement() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter(true, new XmlNamespacePolicy(true, true));
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.closeElement(element, out, 1, "Indent With", true);

    // Assert
    assertEquals("Indent With</foo>\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#closeElement(Element, Writer, int, String, boolean)}.
   * <ul>
   *   <li>Given {@link DOMElementWriter#DOMElementWriter()}.</li>
   *   <li>Then {@link StringWriter#StringWriter()} toString is {@code Indent With</foo>}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#closeElement(Element, Writer, int, String, boolean)}
   */
  @Test
  public void testCloseElement_givenDOMElementWriter_thenStringWriterToStringIsIndentWithFoo() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    IIOMetadataNode element = new IIOMetadataNode("foo");
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.closeElement(element, out, 1, "Indent With", true);

    // Assert
    assertEquals("Indent With</foo>\n", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#encode(String)} with {@code value}.
   * <ul>
   *   <li>Then return {@code ]]&gt;}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encode(String)}
   */
  @Test
  public void testEncodeWithValue_thenReturnGt() {
    // Arrange, Act and Assert
    assertEquals("]]&gt;", (new DOMElementWriter()).encode("]]>"));
  }

  /**
   * Test {@link DOMElementWriter#encode(String)} with {@code value}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encode(String)}
   */
  @Test
  public void testEncodeWithValue_when42_thenReturn42() {
    // Arrange, Act and Assert
    assertEquals("42", (new DOMElementWriter()).encode("42"));
  }

  /**
   * Test {@link DOMElementWriter#encode(String)} with {@code value}.
   * <ul>
   *   <li>When {@code <}.</li>
   *   <li>Then return {@code &lt;}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encode(String)}
   */
  @Test
  public void testEncodeWithValue_whenLessThanSign_thenReturnLt() {
    // Arrange, Act and Assert
    assertEquals("&lt;", (new DOMElementWriter()).encode("<"));
  }

  /**
   * Test {@link DOMElementWriter#encode(String)} with {@code value}.
   * <ul>
   *   <li>When {@code "}.</li>
   *   <li>Then return {@code &quot;}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encode(String)}
   */
  @Test
  public void testEncodeWithValue_whenQuotationMark_thenReturnQuot() {
    // Arrange, Act and Assert
    assertEquals("&quot;", (new DOMElementWriter()).encode("\""));
  }

  /**
   * Test {@link DOMElementWriter#encodedata(Writer, String)} with {@code out}, {@code value}.
   * <ul>
   *   <li>When {@link StringWriter#StringWriter()}.</li>
   *   <li>Then {@link StringWriter#StringWriter()} toString is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodedata(Writer, String)}
   */
  @Test
  public void testEncodedataWithOutValue_whenStringWriter_thenStringWriterToStringIs42() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.encodedata(out, "42");

    // Assert
    assertEquals("42", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#encodedata(Writer, String)} with {@code out}, {@code value}.
   * <ul>
   *   <li>When {@link StringWriter#StringWriter()}.</li>
   *   <li>Then {@link StringWriter#StringWriter()} toString is {@code ]]]]><![CDATA[>}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodedata(Writer, String)}
   */
  @Test
  public void testEncodedataWithOutValue_whenStringWriter_thenStringWriterToStringIsCdata() throws IOException {
    // Arrange
    DOMElementWriter domElementWriter = new DOMElementWriter();
    StringWriter out = new StringWriter();

    // Act
    domElementWriter.encodedata(out, "]]>");

    // Assert
    assertEquals("]]]]><![CDATA[>", out.toString());
  }

  /**
   * Test {@link DOMElementWriter#encodedata(String)} with {@code value}.
   * <ul>
   *   <li>Then return {@code ]]]]><![CDATA[>}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodedata(String)}
   */
  @Test
  public void testEncodedataWithValue_thenReturnCdata() {
    // Arrange, Act and Assert
    assertEquals("]]]]><![CDATA[>", (new DOMElementWriter()).encodedata("]]>"));
  }

  /**
   * Test {@link DOMElementWriter#encodedata(String)} with {@code value}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodedata(String)}
   */
  @Test
  public void testEncodedataWithValue_when42_thenReturn42() {
    // Arrange, Act and Assert
    assertEquals("42", (new DOMElementWriter()).encodedata("42"));
  }

  /**
   * Test {@link DOMElementWriter#isReference(String)}.
   * <ul>
   *   <li>When {@code Ent}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isReference(String)}
   */
  @Test
  public void testIsReference_whenEnt_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new DOMElementWriter()).isReference("Ent"));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenA_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new DOMElementWriter()).isLegalCharacter('A'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When cr.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenCr_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new DOMElementWriter()).isLegalCharacter('\r'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenHighSurrogatesD800_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new DOMElementWriter()).isLegalCharacter('\uD800'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When information separator one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenInformationSeparatorOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new DOMElementWriter()).isLegalCharacter('\u001f'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When lf.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenLf_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new DOMElementWriter()).isLegalCharacter('\n'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When {@code }.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenPrivateUseAreaE000_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new DOMElementWriter()).isLegalCharacter(''));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When tab.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenTab_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new DOMElementWriter()).isLegalCharacter('\t'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalCharacter(char)}.
   * <ul>
   *   <li>When {@code ￾}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter_whenUfffe_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new DOMElementWriter()).isLegalCharacter('￾'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenA_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DOMElementWriter.isLegalXmlCharacter('A'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When cr.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenCr_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DOMElementWriter.isLegalXmlCharacter('\r'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When {@code ?}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenHighSurrogatesD800_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DOMElementWriter.isLegalXmlCharacter('\uD800'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When information separator one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenInformationSeparatorOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DOMElementWriter.isLegalXmlCharacter('\u001f'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When lf.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenLf_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DOMElementWriter.isLegalXmlCharacter('\n'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When {@code }.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenPrivateUseAreaE000_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DOMElementWriter.isLegalXmlCharacter(''));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When tab.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenTab_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(DOMElementWriter.isLegalXmlCharacter('\t'));
  }

  /**
   * Test {@link DOMElementWriter#isLegalXmlCharacter(char)}.
   * <ul>
   *   <li>When {@code ￾}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter_whenUfffe_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(DOMElementWriter.isLegalXmlCharacter('￾'));
  }

  /**
   * Test {@link DOMElementWriter#encodeAttributeValue(String)}.
   * <ul>
   *   <li>Then return {@code ]]&gt;}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodeAttributeValue(String)}
   */
  @Test
  public void testEncodeAttributeValue_thenReturnGt() {
    // Arrange, Act and Assert
    assertEquals("]]&gt;", (new DOMElementWriter()).encodeAttributeValue("]]>"));
  }

  /**
   * Test {@link DOMElementWriter#encodeAttributeValue(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodeAttributeValue(String)}
   */
  @Test
  public void testEncodeAttributeValue_when42_thenReturn42() {
    // Arrange, Act and Assert
    assertEquals("42", (new DOMElementWriter()).encodeAttributeValue("42"));
  }

  /**
   * Test {@link DOMElementWriter#encodeAttributeValue(String)}.
   * <ul>
   *   <li>When {@code <}.</li>
   *   <li>Then return {@code &lt;}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodeAttributeValue(String)}
   */
  @Test
  public void testEncodeAttributeValue_whenLessThanSign_thenReturnLt() {
    // Arrange, Act and Assert
    assertEquals("&lt;", (new DOMElementWriter()).encodeAttributeValue("<"));
  }

  /**
   * Test {@link DOMElementWriter#encodeAttributeValue(String)}.
   * <ul>
   *   <li>When {@code "}.</li>
   *   <li>Then return {@code &quot;}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DOMElementWriter#encodeAttributeValue(String)}
   */
  @Test
  public void testEncodeAttributeValue_whenQuotationMark_thenReturnQuot() {
    // Arrange, Act and Assert
    assertEquals("&quot;", (new DOMElementWriter()).encodeAttributeValue("\""));
  }
}
