package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class DOMElementWriterDiffblueTest {
  /**
   * Method under test: {@link DOMElementWriter#DOMElementWriter()}
   */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals(5, (new DOMElementWriter()).knownEntities.length);
    assertEquals(5, (new DOMElementWriter(true)).knownEntities.length);
    assertEquals(5,
        (new DOMElementWriter(true, new DOMElementWriter.XmlNamespacePolicy(true, true))).knownEntities.length);
  }

  /**
  * Method under test: {@link DOMElementWriter#encode(String)}
  */
  @Test
  public void testEncode() {
    // Arrange, Act and Assert
    assertEquals("42", (new DOMElementWriter()).encode("42"));
    assertEquals("&quot;", (new DOMElementWriter()).encode("\""));
    assertEquals("&lt;", (new DOMElementWriter()).encode("<"));
    assertEquals("]]&gt;", (new DOMElementWriter()).encode("]]>"));
  }

  /**
   * Method under test: {@link DOMElementWriter#encodeAttributeValue(String)}
   */
  @Test
  public void testEncodeAttributeValue() {
    // Arrange, Act and Assert
    assertEquals("42", (new DOMElementWriter()).encodeAttributeValue("42"));
    assertEquals("&quot;", (new DOMElementWriter()).encodeAttributeValue("\""));
    assertEquals("&lt;", (new DOMElementWriter()).encodeAttributeValue("<"));
    assertEquals("]]&gt;", (new DOMElementWriter()).encodeAttributeValue("]]>"));
  }

  /**
   * Method under test: {@link DOMElementWriter#isLegalCharacter(char)}
   */
  @Test
  public void testIsLegalCharacter() {
    // Arrange, Act and Assert
    assertTrue((new DOMElementWriter()).isLegalCharacter('A'));
    assertTrue((new DOMElementWriter()).isLegalCharacter('\t'));
    assertTrue((new DOMElementWriter()).isLegalCharacter('\n'));
    assertTrue((new DOMElementWriter()).isLegalCharacter('\r'));
    assertFalse((new DOMElementWriter()).isLegalCharacter('\u0000'));
  }

  /**
   * Method under test: {@link DOMElementWriter#isLegalXmlCharacter(char)}
   */
  @Test
  public void testIsLegalXmlCharacter() {
    // Arrange, Act and Assert
    assertTrue(DOMElementWriter.isLegalXmlCharacter('A'));
    assertTrue(DOMElementWriter.isLegalXmlCharacter('\t'));
    assertTrue(DOMElementWriter.isLegalXmlCharacter('\n'));
    assertTrue(DOMElementWriter.isLegalXmlCharacter('\r'));
    assertFalse(DOMElementWriter.isLegalXmlCharacter('\u0000'));
  }
}

