package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedOutputStream;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.Properties;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.optional.EchoProperties.FormatAttribute;
import org.apache.tools.ant.types.PropertySet;
import org.junit.Test;

public class EchoPropertiesDiffblueTest {
  /**
   * Test {@link EchoProperties#execute()}.
   * <p>
   * Method under test: {@link EchoProperties#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    echoProperties.setSrcfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    echoProperties.addPropertyset(new PropertySet());

    // Act and Assert
    assertThrows(BuildException.class, () -> echoProperties.execute());
  }

  /**
   * Test {@link EchoProperties#execute()}.
   * <p>
   * Method under test: {@link EchoProperties#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    echoProperties.setDestfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    echoProperties.addPropertyset(new PropertySet());

    // Act and Assert
    assertThrows(BuildException.class, () -> echoProperties.execute());
  }

  /**
   * Test FormatAttribute {@link FormatAttribute#getValues()}.
   * <p>
   * Method under test: {@link FormatAttribute#getValues()}
   */
  @Test
  public void testFormatAttributeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"xml", "text"}, (new FormatAttribute()).getValues());
  }

  /**
   * Test FormatAttribute new {@link FormatAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FormatAttribute}
   */
  @Test
  public void testFormatAttributeNewFormatAttribute() {
    // Arrange and Act
    FormatAttribute actualFormatAttribute = new FormatAttribute();

    // Assert
    assertNull(actualFormatAttribute.getValue());
    assertEquals(-1, actualFormatAttribute.getIndex());
    assertArrayEquals(new String[]{"xml", "text"}, actualFormatAttribute.getValues());
  }

  /**
   * Test {@link EchoProperties#saveProperties(Hashtable, OutputStream)}.
   * <ul>
   *   <li>Then {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one toByteArray is empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EchoProperties#saveProperties(Hashtable, OutputStream)}
   */
  @Test
  public void testSaveProperties_thenByteArrayOutputStreamWithOneToByteArrayIsEmptyArrayOfByte()
      throws IOException, BuildException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    echoProperties.setFormat(new FormatAttribute());
    Hashtable<Object, Object> allProps = new Hashtable<>();
    ByteArrayOutputStream os = new ByteArrayOutputStream(1);

    // Act
    echoProperties.saveProperties(allProps, os);

    // Assert that nothing has changed
    assertArrayEquals(new byte[]{}, os.toByteArray());
  }

  /**
   * Test {@link EchoProperties#saveProperties(Hashtable, OutputStream)}.
   * <ul>
   *   <li>When {@link PipedOutputStream#PipedOutputStream()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EchoProperties#saveProperties(Hashtable, OutputStream)}
   */
  @Test
  public void testSaveProperties_whenPipedOutputStream_thenThrowBuildException() throws IOException, BuildException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    Hashtable<Object, Object> allProps = new Hashtable<>();

    // Act and Assert
    assertThrows(BuildException.class, () -> echoProperties.saveProperties(allProps, new PipedOutputStream()));
  }

  /**
   * Test {@link EchoProperties#xmlSaveProperties(Properties, OutputStream)}.
   * <p>
   * Method under test: {@link EchoProperties#xmlSaveProperties(Properties, OutputStream)}
   */
  @Test
  public void testXmlSaveProperties() throws IOException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    Properties props = new Properties();
    ByteArrayOutputStream os = new ByteArrayOutputStream(1);

    // Act
    echoProperties.xmlSaveProperties(props, os);

    // Assert
    byte[] expectedToByteArrayResult = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><properties />\n".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, os.toByteArray());
  }

  /**
   * Test {@link EchoProperties#xmlSaveProperties(Properties, OutputStream)}.
   * <p>
   * Method under test: {@link EchoProperties#xmlSaveProperties(Properties, OutputStream)}
   */
  @Test
  public void testXmlSaveProperties2() throws IOException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    echoProperties.addPropertyset(new PropertySet());
    Properties props = new Properties();
    ByteArrayOutputStream os = new ByteArrayOutputStream(1);

    // Act
    echoProperties.xmlSaveProperties(props, os);

    // Assert
    byte[] expectedToByteArrayResult = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><properties />\n".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, os.toByteArray());
  }

  /**
   * Test {@link EchoProperties#xmlSaveProperties(Properties, OutputStream)}.
   * <ul>
   *   <li>When {@link PipedOutputStream#PipedOutputStream()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EchoProperties#xmlSaveProperties(Properties, OutputStream)}
   */
  @Test
  public void testXmlSaveProperties_whenPipedOutputStream_thenThrowBuildException() throws IOException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    echoProperties.addPropertyset(new PropertySet());
    echoProperties.addPropertyset(new PropertySet());
    Properties props = new Properties();

    // Act and Assert
    assertThrows(BuildException.class, () -> echoProperties.xmlSaveProperties(props, new PipedOutputStream()));
  }

  /**
   * Test {@link EchoProperties#jdkSaveProperties(Properties, OutputStream, String)}.
   * <ul>
   *   <li>When {@link PipedOutputStream#PipedOutputStream()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EchoProperties#jdkSaveProperties(Properties, OutputStream, String)}
   */
  @Test
  public void testJdkSaveProperties_whenPipedOutputStream_thenThrowBuildException() throws IOException {
    // Arrange
    EchoProperties echoProperties = new EchoProperties();
    Properties props = new Properties();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> echoProperties.jdkSaveProperties(props, new PipedOutputStream(), "Header"));
  }

  /**
   * Test new {@link EchoProperties} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link EchoProperties}
   */
  @Test
  public void testNewEchoProperties() {
    // Arrange and Act
    EchoProperties actualEchoProperties = new EchoProperties();

    // Assert
    Location location = actualEchoProperties.getLocation();
    assertNull(location.getFileName());
    assertNull(actualEchoProperties.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualEchoProperties.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualEchoProperties.getTaskName());
    assertNull(actualEchoProperties.getTaskType());
    assertNull(actualEchoProperties.getProject());
    assertNull(actualEchoProperties.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualEchoProperties, runtimeConfigurableWrapper.getProxy());
  }
}
