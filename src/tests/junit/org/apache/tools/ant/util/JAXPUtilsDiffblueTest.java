package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.SAXParserFactory;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JAXPUtilsDiffblueTest {
  /**
   * Test {@link JAXPUtils#getParserFactory()}.
   * <p>
   * Method under test: {@link JAXPUtils#getParserFactory()}
   */
  @Test
  public void testGetParserFactory() throws BuildException {
    // Arrange and Act
    SAXParserFactory actualParserFactory = JAXPUtils.getParserFactory();

    // Assert
    assertNull(actualParserFactory.getSchema());
    assertFalse(actualParserFactory.isNamespaceAware());
    assertFalse(actualParserFactory.isValidating());
    assertFalse(actualParserFactory.isXIncludeAware());
  }

  /**
   * Test {@link JAXPUtils#getNSParserFactory()}.
   * <p>
   * Method under test: {@link JAXPUtils#getNSParserFactory()}
   */
  @Test
  public void testGetNSParserFactory() throws BuildException {
    // Arrange and Act
    SAXParserFactory actualNSParserFactory = JAXPUtils.getNSParserFactory();

    // Assert
    assertNull(actualNSParserFactory.getSchema());
    assertFalse(actualNSParserFactory.isValidating());
    assertFalse(actualNSParserFactory.isXIncludeAware());
    assertTrue(actualNSParserFactory.isNamespaceAware());
  }

  /**
   * Test {@link JAXPUtils#newParserFactory()}.
   * <p>
   * Method under test: {@link JAXPUtils#newParserFactory()}
   */
  @Test
  public void testNewParserFactory() throws BuildException {
    // Arrange and Act
    SAXParserFactory actualNewParserFactoryResult = JAXPUtils.newParserFactory();

    // Assert
    assertNull(actualNewParserFactoryResult.getSchema());
    assertFalse(actualNewParserFactoryResult.isNamespaceAware());
    assertFalse(actualNewParserFactoryResult.isValidating());
    assertFalse(actualNewParserFactoryResult.isXIncludeAware());
  }

  /**
   * Test {@link JAXPUtils#getSystemId(File)}.
   * <p>
   * Method under test: {@link JAXPUtils#getSystemId(File)}
   */
  @Test
  public void testGetSystemId() {
    // Arrange and Act
    String actualSystemId = JAXPUtils.getSystemId(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(
        String.join("", "file:",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString().concat(File.separator)),
        actualSystemId);
  }

  /**
   * Test {@link JAXPUtils#getDocumentBuilder()}.
   * <p>
   * Method under test: {@link JAXPUtils#getDocumentBuilder()}
   */
  @Test
  public void testGetDocumentBuilder() throws BuildException {
    // Arrange and Act
    DocumentBuilder actualDocumentBuilder = JAXPUtils.getDocumentBuilder();

    // Assert
    assertNull(actualDocumentBuilder.getSchema());
    assertFalse(actualDocumentBuilder.isXIncludeAware());
  }
}
