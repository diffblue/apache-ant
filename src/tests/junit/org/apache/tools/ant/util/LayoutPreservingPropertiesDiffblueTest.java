package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import org.junit.Test;

public class LayoutPreservingPropertiesDiffblueTest {
  /**
   * Test {@link LayoutPreservingProperties#LayoutPreservingProperties()}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#LayoutPreservingProperties()}
   */
  @Test
  public void testNewLayoutPreservingProperties() {
    // Arrange, Act and Assert
    assertTrue((new LayoutPreservingProperties()).isEmpty());
  }

  /**
   * Test {@link LayoutPreservingProperties#LayoutPreservingProperties(Properties)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#LayoutPreservingProperties(Properties)}
   */
  @Test
  public void testNewLayoutPreservingProperties2() {
    // Arrange
    Properties defaults = new Properties();

    // Act and Assert
    assertEquals(defaults, new LayoutPreservingProperties(defaults));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LayoutPreservingProperties#setRemoveComments(boolean)}
   *   <li>{@link LayoutPreservingProperties#isRemoveComments()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    layoutPreservingProperties.setRemoveComments(true);

    // Assert
    assertTrue(layoutPreservingProperties.isRemoveComments());
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\r\nAXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("AXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream2() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\n\\AXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("AXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream3() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    layoutPreservingProperties.load(new ByteArrayInputStream(new byte[]{1, '\r', 'A', 'X', 'A', 'X', 'A', 'X'}));

    // Assert
    assertEquals(2, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("AXAXAX"));
    assertEquals("", layoutPreservingProperties.get("\u0001"));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream4() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\\\nAXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("AXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code A} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesAIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    layoutPreservingProperties.load(new ByteArrayInputStream("A\rAXAXAX".getBytes("UTF-8")));

    // Assert
    assertEquals(2, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("A"));
    assertEquals("", layoutPreservingProperties.get("AXAXAX"));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \AXAXAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesAxaxaxIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\\\\AXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\AXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code AXAXAXAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesAxaxaxaxIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("AXAXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} containsKey {@code Key}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesContainsKeyKey()
      throws IOException, NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");
    ByteArrayInputStream inStream = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(2, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("AXAXAXAX"));
    assertTrue(layoutPreservingProperties.containsKey("Key"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesEmpty() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream(new byte[]{});

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert that nothing has changed
    assertTrue(layoutPreservingProperties.isEmpty());
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code XAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesXaxIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\rXAX\rXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("XAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code XAXAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesXaxaxIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\rXA\\\rXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("XAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \XAXAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesXaxaxIsEmptyString2() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\n\\\\XAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\XAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code XAXAXA} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesXaxaxaIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\nXAXAXA\\".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("XAXAXA"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code XAXAXAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesXaxaxaxIsEmptyString() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\rXAXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("XAXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#load(InputStream)} with {@code InputStream}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code XAXAXAX} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#load(InputStream)}
   */
  @Test
  public void testLoadWithInputStream_thenLayoutPreservingPropertiesXaxaxaxIsEmptyString2() throws IOException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    ByteArrayInputStream inStream = new ByteArrayInputStream("\nXAXAXAX".getBytes("UTF-8"));

    // Act
    layoutPreservingProperties.load(inStream);

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("XAXAXAX"));
    assertEquals(Retryable.RETRY_FOREVER, inStream.read(new byte[]{}));
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut3() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\\\\\", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut4() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut5() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut6() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut7() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut8() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut9() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut10() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut11() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut12() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut13() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut14() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut15() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut16() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut17() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut18() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut19() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut20() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut21() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut22() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut23() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut24() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut25() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut26() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut27() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut28() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut29() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut30() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut31() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut32() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut33() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut34() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut35() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\t\f\r\n\\:=#!", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("\t\f\r\n\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut36() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut37() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties",
        layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut38() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\t\f\r\n\\:=#!", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut39() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut40() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut41() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut43() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut44() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("org.apache.tools.ant.util.LayoutPreservingProperties",
        "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut45() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("org.apache.tools.ant.util.LayoutPreservingProperties",
        "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut46() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("org.apache.tools.ant.util.LayoutPreservingProperties",
        "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut47() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("org.apache.tools.ant.util.LayoutPreservingProperties",
        "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut48() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("org.apache.tools.ant.util.LayoutPreservingProperties",
        "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut49() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut50() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(9, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(9));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut51() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut52() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut53() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut54() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(JavaEnvUtils.VERSION_1_2, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(JavaEnvUtils.VERSION_1_2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut55() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut56() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(2, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut57() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(JavaEnvUtils.VERSION_1_3, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(JavaEnvUtils.VERSION_1_3));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut58() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut59() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("java.lang.Integer", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut60() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(3, "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(3));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut61() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut62() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("org.apache.tools.ant.util.LayoutPreservingProperties",
        "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Given {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code Value}.</li>
   *   <li>When {@code Key}.</li>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_givenLayoutPreservingPropertiesKeyIsValue_whenKey_thenReturnValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", actualPutResult);
    assertTrue(layoutPreservingProperties.containsKey("Key"));
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Given {@link LayoutPreservingProperties#LayoutPreservingProperties()}.</li>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_givenLayoutPreservingProperties_whenT_thenLayoutPreservingPropertiesTIsT()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsEightySix()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsFortyTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsMin_value()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsNinetySeven()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsRetry_forever()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashBackslashIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIntValueIsEightySix() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIntValueIsFortyTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIntValueIsMin_value() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIntValueIsNinetySeven() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIntValueIsRetry_forever()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashColonIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsEightySix()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsFortyTwo()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsMin_value()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsNinetySeven()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsRetry_forever()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashEqualsSignIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIntValueIsNinetySeven() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is one hundred twenty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIntValueIsOneHundredTwentySix()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIntValueIsRetry_forever() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsBackslashBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsBackslashColon2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsBackslashEqualsSign2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesBackslashIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEmptyStringIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEmptyStringIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEmptyStringIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIntValueIsEightySix() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIntValueIsMin_value() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIntValueIsNinetySeven() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is one hundred twenty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIntValueIsOneHundredTwentySix()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIntValueIsRetry_forever() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesEqualsSignIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesJavaLangIntegerIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("java.lang.Integer", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesJavaLangIntegerIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("java.lang.Integer", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesJavaLangIntegerIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("java.lang.Integer", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesJavaLangIntegerIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("java.lang.Integer", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \\\\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesKeyIsBackslashBackslashBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#MIN_VALUE} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesMin_valueIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#MIN_VALUE} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesMin_valueIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} ninety-two is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesNinetyTwoIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(92, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(92));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} ninety-two is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesNinetyTwoIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(92, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(92));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one hundred sixteen is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesOneHundredSixteenIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(116, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(116));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one hundred twenty-six is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesOneHundredTwentySixIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(126, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(126));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one hundred twenty-six is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesOneHundredTwentySixIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(126, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(126));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one hundred twenty-six is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesOneHundredTwentySixIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(126, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(126));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Retryable#RETRY_FOREVER} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesRetry_foreverIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Retryable#RETRY_FOREVER} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesRetry_foreverIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Retryable#RETRY_FOREVER} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesRetry_foreverIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} seventy-five is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesSeventyFiveIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(75, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(75));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is one hundred twenty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesTIntValueIsOneHundredTwentySix() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is one hundred twenty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesTfrnIntValueIsOneHundredTwentySix() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 126));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(126, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_thenLayoutPreservingPropertiesTfrnIntValueIsRetry_forever() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \:}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenBackslashColon_thenLayoutPreservingPropertiesBackslashColonIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenBackslash_thenLayoutPreservingPropertiesBackslashIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenBackslash_thenLayoutPreservingPropertiesBackslashIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenBackslash_thenLayoutPreservingPropertiesBackslashIsBackslash3() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When eighty-six.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEightySix_thenLayoutPreservingPropertiesBackslashIntValueIsEightySix()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When eighty-six.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEightySix_thenLayoutPreservingPropertiesTIntValueIsEightySix() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When eighty-six.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is eighty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEightySix_thenLayoutPreservingPropertiesTfrnIntValueIsEightySix()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 86));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(86, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesBackslashIsEmptyString()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIsBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIsEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesEqualsSignIsEmptyString()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesTIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEmptyString_thenLayoutPreservingPropertiesTfrnIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsBackslash2()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When fifty-eight.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} fifty-eight is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFiftyEight_thenLayoutPreservingPropertiesFiftyEightIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(58, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(58));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsFive()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesBackslashColonIntValueIsFive()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsFive()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesBackslashIntValueIsFive() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesEqualsSignIntValueIsFive() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} five is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesFiveIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(5, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(5));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesTIntValueIsFive() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is five.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFive_thenLayoutPreservingPropertiesTfrnIntValueIsFive() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 5));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(5, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesBackslashIntValueIsFortyTwo()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesEqualsSignIntValueIsFortyTwo()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} forty-two is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesFortyTwoIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} forty-two is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesFortyTwoIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} forty-two is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesFortyTwoIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} forty-two is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesFortyTwoIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} forty-two is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesFortyTwoIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} forty-two is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesFortyTwoIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(42, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(42));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesTIntValueIsFortyTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFortyTwo_thenLayoutPreservingPropertiesTfrnIntValueIsFortyTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 42));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(42, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When four.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} four is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFour_thenLayoutPreservingPropertiesFourIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(4, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(4));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When four.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} four is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFour_thenLayoutPreservingPropertiesFourIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(4, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(4));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When four.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} four is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFour_thenLayoutPreservingPropertiesFourIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(4, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(4));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When four.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} four is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFour_thenLayoutPreservingPropertiesFourIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(4, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(4));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When four.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} four is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenFour_thenLayoutPreservingPropertiesFourIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(4, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(4));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Integer}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenJavaLangInteger_thenLayoutPreservingPropertiesJavaLangIntegerIsT()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("java.lang.Integer", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Integer}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenJavaLangInteger_thenLayoutPreservingPropertiesTIsJavaLangInteger()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Integer}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenJavaLangInteger_thenLayoutPreservingPropertiesTfrnIsJavaLangInteger()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenKey_thenLayoutPreservingPropertiesKeyIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("Key", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("Key"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesBackslashIntValueIsMin_value()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#MIN_VALUE} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesMin_valueIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#MIN_VALUE} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesMin_valueIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#MIN_VALUE} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesMin_valueIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#MIN_VALUE} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesMin_valueIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.MIN_VALUE, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(Integer.MIN_VALUE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesTIntValueIsMin_value() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#MIN_VALUE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is {@link Integer#MIN_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenMin_value_thenLayoutPreservingPropertiesTfrnIntValueIsMin_value()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", Integer.MIN_VALUE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.MIN_VALUE, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When nine.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} nine is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenNine_thenLayoutPreservingPropertiesNineIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(9, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(9));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When ninety-seven.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenNinetySeven_thenLayoutPreservingPropertiesTIntValueIsNinetySeven()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When ninety-seven.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is ninety-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenNinetySeven_thenLayoutPreservingPropertiesTfrnIntValueIsNinetySeven()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 97));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(97, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When ninety-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} ninety-two is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenNinetyTwo_thenLayoutPreservingPropertiesNinetyTwoIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(92, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(92));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When ninety-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} ninety-two is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenNinetyTwo_thenLayoutPreservingPropertiesNinetyTwoIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(92, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(92));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When ninety-two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} ninety-two is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenNinetyTwo_thenLayoutPreservingPropertiesNinetyTwoIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(92, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(92));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one hundred sixteen.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one hundred sixteen is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOneHundredSixteen_thenLayoutPreservingPropertiesOneHundredSixteenIsT()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(116, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(116));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one hundred two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one hundred two is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOneHundredTwo_thenLayoutPreservingPropertiesOneHundredTwoIsTfrn()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(102, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(102));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsOne()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesBackslashColonIntValueIsOne() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsOne()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesBackslashIntValueIsOne() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesEqualsSignIntValueIsOne() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesOneIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesOneIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesOneIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesOneIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesOneIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} one is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesOneIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(1, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(1));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesTIntValueIsOne() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenOne_thenLayoutPreservingPropertiesTfrnIntValueIsOne() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 1));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(1, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Retryable#RETRY_FOREVER}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Retryable#RETRY_FOREVER} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenRetry_forever_thenLayoutPreservingPropertiesRetry_foreverIsBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Retryable#RETRY_FOREVER}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Retryable#RETRY_FOREVER} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenRetry_forever_thenLayoutPreservingPropertiesRetry_foreverIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Retryable#RETRY_FOREVER}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Retryable#RETRY_FOREVER} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenRetry_forever_thenLayoutPreservingPropertiesRetry_foreverIsTfrn()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Retryable.RETRY_FOREVER, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(Retryable.RETRY_FOREVER));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Retryable#RETRY_FOREVER}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenRetry_forever_thenLayoutPreservingPropertiesTIntValueIsRetry_forever()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", Retryable.RETRY_FOREVER));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Retryable.RETRY_FOREVER, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When six.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} six is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSix_thenLayoutPreservingPropertiesSixIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(6, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(6));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When sixty-one.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} sixty-one is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSixtyOne_thenLayoutPreservingPropertiesSixtyOneIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(61, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(61));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsSize()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesBackslashColonIntValueIsSize()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsSize()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesBackslashIntValueIsSize() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesEqualsSignIntValueIsSize() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#SIZE} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesSizeIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#SIZE} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesSizeIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#SIZE} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesSizeIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#SIZE} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesSizeIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#SIZE} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesSizeIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@link Integer#SIZE} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesSizeIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(Integer.SIZE, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(Integer.SIZE));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesTIntValueIsSize() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@link Integer#SIZE}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is {@link Integer#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenSize_thenLayoutPreservingPropertiesTfrnIntValueIsSize() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", Integer.SIZE));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(Integer.SIZE, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesBackslashIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesBackslashIsT2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesTIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesTIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesTIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesTIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenT_thenLayoutPreservingPropertiesTIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesBackslashBackslashIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesBackslashColonIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesBackslashEqualsSignIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesBackslashIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTfrn_thenLayoutPreservingPropertiesTfrnIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} three is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenThree_thenLayoutPreservingPropertiesThreeIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(3, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(3));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} three is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenThree_thenLayoutPreservingPropertiesThreeIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(3, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(3));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsTwo()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesBackslashColonIntValueIsTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsTwo()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesBackslashIntValueIsTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesEqualsSignIntValueIsTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTIntValueIsTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTfrnIntValueIsTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 2));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(2, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} two is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTwoIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(2, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} two is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTwoIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(2, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} two is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTwoIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(2, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} two is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTwoIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(2, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When two.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} two is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenTwo_thenLayoutPreservingPropertiesTwoIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(2, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(2));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesBackslashBackslashIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\\\", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\\\"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesBackslashColonIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\:", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\:"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesBackslashEqualsSignIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\=", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesBackslashIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\ ", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\ "));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesEmptyStringIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get(""));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesEqualsSignIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("=", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("="));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesTIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("\\t", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\t"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenValue_thenLayoutPreservingPropertiesTfrnIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put("tfrn\\:=#!", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesBackslashBackslashIntValueIsZero()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\\\", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("\\\\")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesBackslashColonIntValueIsZero()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\:", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("\\:")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesBackslashEqualsSignIntValueIsZero()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\=", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("\\=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesBackslashIntValueIsZero() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\ ", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("\\ ")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesEqualsSignIntValueIsZero() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("=", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("=")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesTIntValueIsZero() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("\\t", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("\\t")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} intValue is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesTfrnIntValueIsZero() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.put("tfrn\\:=#!", 0));
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals(0, ((Integer) layoutPreservingProperties.get("tfrn\\:=#!")).intValue());
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} zero is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesZeroIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} zero is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesZeroIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} zero is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesZeroIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} zero is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesZeroIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} zero is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesZeroIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#put(Object, Object)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} zero is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut_whenZero_thenLayoutPreservingPropertiesZeroIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualPutResult = layoutPreservingProperties.put(0, "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(0));
    assertNull(actualPutResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty3() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty4() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty5() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty6() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty7() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty8() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty9() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty10() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty11() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty12() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty13() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty14() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty15() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty16() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty17() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty18() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty19() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty20() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty21() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty22() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty23() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty24() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty25() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty26() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty27() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty28() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty29() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty30() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty31() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty32() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty33() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty34() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty35() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty36() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty37() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty38() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties",
        layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty39() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty40() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty41() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty43() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty44() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty45() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty46() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty47() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty48() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty49() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty50() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty51() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty52() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty53() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty54() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty55() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty56() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty57() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty58() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty59() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty60() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty61() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty62() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty63() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine",
        layoutPreservingProperties.get("\\\\\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty64() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty65() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty66() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty67() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty68() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty69() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty70() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty71() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty72() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty73() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty74() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty75() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty76() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty77() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty78() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty79() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty80() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\t\f\r\n\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\t\f\r\n\\:=#!",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty81() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty82() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty83() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty84() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty85() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty86() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty87() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty88() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty89() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty90() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\t\f\r\n\\:=#!", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\t\f\r\n\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty91() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty92() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty93() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty94() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty95() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty96() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty97() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\\\\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\\\\\",
        layoutPreservingProperties.get("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty98() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties$Pair");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("org.apache.tools.ant.util.LayoutPreservingProperties$Pair", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashBackslashIsBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashBackslashIsBackslash2()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashBackslashIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashBackslashIsEmptyString()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashBackslashIsEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashBackslashIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsJavaLangInteger()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashColonIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsBackslash2()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsEmptyString()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashEqualsSignIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsBackslashBackslash2()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsBackslashColon2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsBackslashEqualsSign2()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsEqualsSign2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesBackslashIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEmptyStringIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEmptyStringIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEmptyStringIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEmptyStringIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEmptyStringIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsEmptyString() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesEqualsSignIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesJavaLangIntegerIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesJavaLangIntegerIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesJavaLangIntegerIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesJavaLangIntegerIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Integer} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesJavaLangIntegerIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("java.lang.Integer", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("java.lang.Integer"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesTIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code Integer}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_thenLayoutPreservingPropertiesTfrnIsJavaLangInteger() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "java.lang.Integer");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("java.lang.Integer", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code 42} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingProperties42IsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("42", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("42"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesBackslashBackslashIs42()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesBackslashColonIs42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \=} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesBackslashEqualsSignIs42()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\=", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\\="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesBackslashIs42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesEqualsSignIs42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesTIs42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_when42_thenLayoutPreservingPropertiesTfrnIs42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenBackslash_thenLayoutPreservingPropertiesBackslashIsBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenBackslash_thenLayoutPreservingPropertiesBackslashIsBackslash2()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenBackslash_thenLayoutPreservingPropertiesBackslashIsBackslash3()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIs42()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIsT()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} empty string is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEmptyString_thenLayoutPreservingPropertiesEmptyStringIsTfrn()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get(""));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEmptyString_thenLayoutPreservingPropertiesTIsEmptyString()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEmptyString_thenLayoutPreservingPropertiesTfrnIsEmptyString()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code =}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenEqualsSign_thenLayoutPreservingPropertiesEqualsSignIsTfrn()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIs42() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "42");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("42", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Key}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenKey_thenLayoutPreservingPropertiesKeyIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("Key", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("Key"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesBackslashBackslashIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesBackslashIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesBackslashIsT2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsBackslashBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsBackslashColon() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsBackslashEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code \t}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenT_thenLayoutPreservingPropertiesTIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \\} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesBackslashBackslashIsTfrn()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\\\", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesBackslashIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesBackslashIsTfrn2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("\\"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslash() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\ ");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\ ", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslash2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \\}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslashBackslash()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\\\");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\\\", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslashColon()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\:");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\:", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsBackslashEqualsSign()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\=", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code =}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsEqualsSign() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "=");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("=", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code \t}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsT() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "\\t");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("\\t", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code tfrn\:=#!}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code tfrn\:=#!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenTfrn_thenLayoutPreservingPropertiesTfrnIsTfrn() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "tfrn\\:=#!");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("tfrn\\:=#!", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \:} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenValue_thenLayoutPreservingPropertiesBackslashColonIsValue()
      throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\:", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\:"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenValue_thenLayoutPreservingPropertiesBackslashIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\ ", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\ "));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code =} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenValue_thenLayoutPreservingPropertiesEqualsSignIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("=", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("="));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code \t} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenValue_thenLayoutPreservingPropertiesTIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("\\t", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("\\t"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code tfrn\:=#!} is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenValue_thenLayoutPreservingPropertiesTfrnIsValue() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act
    Object actualSetPropertyResult = layoutPreservingProperties.setProperty("tfrn\\:=#!", "Value");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", layoutPreservingProperties.get("tfrn\\:=#!"));
    assertNull(actualSetPropertyResult);
  }

  /**
   * Test {@link LayoutPreservingProperties#remove(Object)} with {@code Object}.
   * <ul>
   *   <li>Given {@link LayoutPreservingProperties#LayoutPreservingProperties()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemoveWithObject_givenLayoutPreservingProperties_thenReturnNull() {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertNull(layoutPreservingProperties.remove("Key"));
    assertTrue(layoutPreservingProperties.isEmpty());
  }

  /**
   * Test {@link LayoutPreservingProperties#remove(Object)} with {@code Object}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemoveWithObject_thenLayoutPreservingPropertiesEmpty() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals("Value", layoutPreservingProperties.remove("Key"));
    assertTrue(layoutPreservingProperties.isEmpty());
  }

  /**
   * Test {@link LayoutPreservingProperties#remove(Object)} with {@code Object}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemoveWithObject_thenLayoutPreservingPropertiesSizeIsOne() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put(42, "Value");
    layoutPreservingProperties.setRemoveComments(true);
    layoutPreservingProperties.put("Key", "Value");

    // Act
    Object actualRemoveResult = layoutPreservingProperties.remove("Key");

    // Assert
    assertEquals(1, layoutPreservingProperties.size());
    assertEquals("Value", actualRemoveResult);
    assertTrue(layoutPreservingProperties.containsKey(42));
  }

  /**
   * Test {@link LayoutPreservingProperties#remove(Object)} with {@code Object}.
   * <ul>
   *   <li>Then {@link LayoutPreservingProperties#LayoutPreservingProperties()} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemoveWithObject_thenLayoutPreservingPropertiesSizeIsTwo() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put(1, "Value");
    layoutPreservingProperties.put(42, "Value");
    layoutPreservingProperties.setRemoveComments(true);
    layoutPreservingProperties.put("Key", "Value");

    // Act
    Object actualRemoveResult = layoutPreservingProperties.remove("Key");

    // Assert
    assertEquals(2, layoutPreservingProperties.size());
    assertEquals("Value", actualRemoveResult);
    assertTrue(layoutPreservingProperties.containsKey(1));
    assertTrue(layoutPreservingProperties.containsKey(42));
  }

  /**
   * Test {@link LayoutPreservingProperties#clone()}.
   * <ul>
   *   <li>Given {@link LayoutPreservingProperties#LayoutPreservingProperties()} {@code Key} is {@code Value}.</li>
   *   <li>Then return {@link Map}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#clone()}
   */
  @Test
  public void testClone_givenLayoutPreservingPropertiesKeyIsValue_thenReturnMap() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");

    // Act
    Object actualCloneResult = layoutPreservingProperties.clone();

    // Assert
    assertTrue(actualCloneResult instanceof Map);
    assertEquals(1, ((Map<String, String>) actualCloneResult).size());
    assertEquals("Value", ((Map<String, String>) actualCloneResult).get("Key"));
  }

  /**
   * Test {@link LayoutPreservingProperties#clone()}.
   * <ul>
   *   <li>Given {@link LayoutPreservingProperties#LayoutPreservingProperties()}.</li>
   *   <li>Then return {@link LayoutPreservingProperties#LayoutPreservingProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LayoutPreservingProperties#clone()}
   */
  @Test
  public void testClone_givenLayoutPreservingProperties_thenReturnLayoutPreservingProperties() {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();

    // Act and Assert
    assertEquals(layoutPreservingProperties, layoutPreservingProperties.clone());
  }
}
