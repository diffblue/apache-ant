package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import javax.xml.parsers.SAXParser;
import org.apache.tools.ant.taskdefs.optional.ejb.IPlanetEjbc.EjbcException;
import org.junit.Test;
import org.xml.sax.SAXException;

public class IPlanetEjbcDiffblueTest {
  /**
   * Test EjbcException {@link EjbcException#EjbcException(IPlanetEjbc, String)}.
   * <p>
   * Method under test: {@link EjbcException#EjbcException(IPlanetEjbc, String)}
   */
  @Test
  public void testEjbcExceptionNewEjbcException() {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    EjbcException actualEjbcException = (new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Classpath", null)).new EjbcException(
            "Msg");

    // Assert
    assertEquals("Msg", actualEjbcException.getMessage());
    assertNull(actualEjbcException.getCause());
    assertEquals(0, actualEjbcException.getSuppressed().length);
  }

  /**
   * Test {@link IPlanetEjbc#IPlanetEjbc(File, File, File, String, SAXParser)}.
   * <ul>
   *   <li>When {@code Classpath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbc#IPlanetEjbc(File, File, File, String, SAXParser)}
   */
  @Test
  public void testNewIPlanetEjbc_whenClasspath() {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    IPlanetEjbc actualIPlanetEjbc = new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Classpath", null);

    // Assert
    assertNull(actualIPlanetEjbc.getDisplayName());
    assertEquals(0, actualIPlanetEjbc.getCmpDescriptors().length);
    assertTrue(actualIPlanetEjbc.getEjbFiles().isEmpty());
  }

  /**
   * Test {@link IPlanetEjbc#IPlanetEjbc(File, File, File, String, SAXParser)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbc#IPlanetEjbc(File, File, File, String, SAXParser)}
   */
  @Test
  public void testNewIPlanetEjbc_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    IPlanetEjbc actualIPlanetEjbc = new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null, null);

    // Assert
    assertNull(actualIPlanetEjbc.getDisplayName());
    assertEquals(0, actualIPlanetEjbc.getCmpDescriptors().length);
    assertTrue(actualIPlanetEjbc.getEjbFiles().isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link IPlanetEjbc#setDebugOutput(boolean)}
   *   <li>{@link IPlanetEjbc#setIasHomeDir(File)}
   *   <li>{@link IPlanetEjbc#setRetainSource(boolean)}
   *   <li>{@link IPlanetEjbc#getDisplayName()}
   *   <li>{@link IPlanetEjbc#getEjbFiles()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    IPlanetEjbc iPlanetEjbc = new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Classpath", null);

    // Act
    iPlanetEjbc.setDebugOutput(true);
    iPlanetEjbc.setIasHomeDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    iPlanetEjbc.setRetainSource(true);
    String actualDisplayName = iPlanetEjbc.getDisplayName();

    // Assert
    assertNull(actualDisplayName);
    assertTrue(iPlanetEjbc.getEjbFiles().isEmpty());
  }

  /**
   * Test {@link IPlanetEjbc#getCmpDescriptors()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#getCmpDescriptors()}
   */
  @Test
  public void testGetCmpDescriptors() {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        (new IPlanetEjbc(stdDescriptor, iasDescriptor,
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Classpath", null))
            .getCmpDescriptors().length);
  }

  /**
   * Test {@link IPlanetEjbc#execute()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#execute()}
   */
  @Test
  public void testExecute() throws IOException, EjbcException, SAXException {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(EjbcException.class, () -> (new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Classpath", null)).execute());
  }

  /**
   * Test {@link IPlanetEjbc#execute()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#execute()}
   */
  @Test
  public void testExecute2() throws IOException, EjbcException, SAXException {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(EjbcException.class,
        () -> (new IPlanetEjbc(stdDescriptor, iasDescriptor,
            Paths.get(System.getProperty("java.io.tmpdir"), "An XML parser must be specified.    ").toFile(),
            "Classpath", null)).execute());
  }

  /**
   * Test {@link IPlanetEjbc#execute()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#execute()}
   */
  @Test
  public void testExecute3() throws IOException, EjbcException, SAXException {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(EjbcException.class, () -> (new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null, null)).execute());
  }

  /**
   * Test {@link IPlanetEjbc#checkConfiguration()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration() throws EjbcException {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(EjbcException.class,
        () -> (new IPlanetEjbc(stdDescriptor, iasDescriptor,
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Classpath", null))
            .checkConfiguration());
  }

  /**
   * Test {@link IPlanetEjbc#checkConfiguration()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration2() throws EjbcException {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(EjbcException.class,
        () -> (new IPlanetEjbc(stdDescriptor, iasDescriptor,
            Paths.get(System.getProperty("java.io.tmpdir"), "An XML parser must be specified.    ").toFile(),
            "Classpath", null)).checkConfiguration());
  }

  /**
   * Test {@link IPlanetEjbc#checkConfiguration()}.
   * <p>
   * Method under test: {@link IPlanetEjbc#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration3() throws EjbcException {
    // Arrange
    File stdDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    File iasDescriptor = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(EjbcException.class, () -> (new IPlanetEjbc(stdDescriptor, iasDescriptor,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null, null)).checkConfiguration());
  }
}
