package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Properties;
import org.junit.Test;

public class LayoutPreservingPropertiesDiffblueTest {
  /**
   * Method under test: {@link LayoutPreservingProperties#clone()}
   */
  @Test
  public void testClone() {
    // Arrange, Act and Assert
    assertTrue(((LayoutPreservingProperties) (new LayoutPreservingProperties()).clone()).isEmpty());
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#clone()}
   */
  @Test
  public void testClone2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals(1, ((LayoutPreservingProperties) layoutPreservingProperties.clone()).size());
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#clone()}
   */
  @Test
  public void testClone3() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");
    layoutPreservingProperties.remove("Key");
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals(1, ((LayoutPreservingProperties) layoutPreservingProperties.clone()).size());
  }

  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link LayoutPreservingProperties#LayoutPreservingProperties()}
  *   <li>{@link LayoutPreservingProperties#setRemoveComments(boolean)}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    LayoutPreservingProperties actualLayoutPreservingProperties = new LayoutPreservingProperties();
    actualLayoutPreservingProperties.setRemoveComments(true);

    // Assert
    assertTrue(actualLayoutPreservingProperties.isEmpty());
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#LayoutPreservingProperties()}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertTrue((new LayoutPreservingProperties()).isEmpty());
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#LayoutPreservingProperties(Properties)}
   */
  @Test
  public void testConstructor3() {
    // Arrange, Act and Assert
    assertTrue((new LayoutPreservingProperties(new Properties())).isEmpty());
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut() throws NullPointerException {
    // Arrange, Act and Assert
    assertNull((new LayoutPreservingProperties()).put("Key", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\\=", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\\:", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\\t", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "Value"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "Value"));
    assertNull((new LayoutPreservingProperties()).put("=", "Value"));
    assertNull((new LayoutPreservingProperties()).put("", "Value"));
    assertNull((new LayoutPreservingProperties()).put("Key", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("Key", "\\="));
    assertNull((new LayoutPreservingProperties()).put("Key", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("Key", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("Key", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("Key", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("Key", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("Key", "="));
    assertNull((new LayoutPreservingProperties()).put("\\\\\\\\", "Value"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 42));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 1));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 5));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\\="));
    assertNull((new LayoutPreservingProperties()).put("\\\\", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 86));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("\\\\", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 0));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 126));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 97));
    assertNull((new LayoutPreservingProperties()).put("\\\\", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", 2));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "\\"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("\\\\", ""));
    assertNull((new LayoutPreservingProperties()).put("\\=", 42));
    assertNull((new LayoutPreservingProperties()).put("\\=", 1));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\\=", 5));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\\="));
    assertNull((new LayoutPreservingProperties()).put("\\=", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("\\=", 86));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("\\=", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("\\=", 0));
    assertNull((new LayoutPreservingProperties()).put("\\=", 126));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\=", 97));
    assertNull((new LayoutPreservingProperties()).put("\\=", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("\\=", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\=", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("\\=", 2));
    assertNull((new LayoutPreservingProperties()).put("\\=", "\\"));
    assertNull((new LayoutPreservingProperties()).put("\\=", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("\\=", ""));
    assertNull((new LayoutPreservingProperties()).put("\\:", 42));
    assertNull((new LayoutPreservingProperties()).put("\\:", 1));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\\:", 5));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\\="));
    assertNull((new LayoutPreservingProperties()).put("\\:", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("\\:", 86));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("\\:", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("\\:", 0));
    assertNull((new LayoutPreservingProperties()).put("\\:", 126));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\:", 97));
    assertNull((new LayoutPreservingProperties()).put("\\:", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("\\:", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\:", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("\\:", 2));
    assertNull((new LayoutPreservingProperties()).put("\\:", "\\"));
    assertNull((new LayoutPreservingProperties()).put("\\:", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("\\:", ""));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 42));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 1));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 5));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\\="));
    assertNull((new LayoutPreservingProperties()).put("\\ ", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 86));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("\\ ", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 0));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 126));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 97));
    assertNull((new LayoutPreservingProperties()).put("\\ ", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", 2));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "\\"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("\\ ", ""));
    assertNull((new LayoutPreservingProperties()).put("\\t", 42));
    assertNull((new LayoutPreservingProperties()).put("\\t", 1));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\\t", 5));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\\="));
    assertNull((new LayoutPreservingProperties()).put("\\t", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("\\t", 86));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("\\t", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("\\t", 0));
    assertNull((new LayoutPreservingProperties()).put("\\t", 126));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\t", 97));
    assertNull((new LayoutPreservingProperties()).put("\\t", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("\\t", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\\t", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("\\t", 2));
    assertNull((new LayoutPreservingProperties()).put("\\t", "\\"));
    assertNull((new LayoutPreservingProperties()).put("\\t", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("\\t", ""));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 42));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 1));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 5));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\\="));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 86));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 0));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 126));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 97));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", 2));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", "\\"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("\t\f\r\n\\:=#!", ""));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 42));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 1));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 5));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\\="));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 86));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 0));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 126));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 97));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", 2));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).put("tfrn\\:=#!", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("tfrn\\:=#!", ""));
    assertNull((new LayoutPreservingProperties()).put("=", 42));
    assertNull((new LayoutPreservingProperties()).put("=", 1));
    assertNull((new LayoutPreservingProperties()).put("=", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("=", 5));
    assertNull((new LayoutPreservingProperties()).put("=", "\\="));
    assertNull((new LayoutPreservingProperties()).put("=", Integer.SIZE));
    assertNull((new LayoutPreservingProperties()).put("=", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("=", 86));
    assertNull((new LayoutPreservingProperties()).put("=", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("=", Retryable.RETRY_FOREVER));
    assertNull((new LayoutPreservingProperties()).put("=", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("=", 0));
    assertNull((new LayoutPreservingProperties()).put("=", 126));
    assertNull((new LayoutPreservingProperties()).put("=", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("=", 97));
    assertNull((new LayoutPreservingProperties()).put("=", Integer.MIN_VALUE));
    assertNull((new LayoutPreservingProperties()).put("=", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("=", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).put("=", 2));
    assertNull((new LayoutPreservingProperties()).put("=", "\\"));
    assertNull((new LayoutPreservingProperties()).put("=", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).put("=", ""));
    assertNull((new LayoutPreservingProperties()).put("", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("", "\\="));
    assertNull((new LayoutPreservingProperties()).put("", "\\:"));
    assertNull((new LayoutPreservingProperties()).put("", "\\ "));
    assertNull((new LayoutPreservingProperties()).put("", "\\t"));
    assertNull((new LayoutPreservingProperties()).put("", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).put("", "="));
    assertNull((new LayoutPreservingProperties()).put("Key", "\\\\\\\\"));
    assertNull((new LayoutPreservingProperties()).put(1, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(2, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(42, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(Integer.SIZE, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(92, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(Retryable.RETRY_FOREVER, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(0, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(4, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(3, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(Integer.MIN_VALUE, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("java.lang.Integer", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put(75, "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("\\", "\\\\"));
    assertNull((new LayoutPreservingProperties()).put("org.apache.tools.ant.util.LayoutPreservingProperties", "\\\\"));
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#put(Object, Object)}
   */
  @Test
  public void testPut2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals("Value", layoutPreservingProperties.put("Key", "Value"));
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemove() {
    // Arrange, Act and Assert
    assertNull((new LayoutPreservingProperties()).remove("Key"));
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemove2() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals("Value", layoutPreservingProperties.remove("Key"));
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemove3() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put(42, "Value");
    layoutPreservingProperties.setRemoveComments(true);
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals("Value", layoutPreservingProperties.remove("Key"));
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#remove(Object)}
   */
  @Test
  public void testRemove4() throws NullPointerException {
    // Arrange
    LayoutPreservingProperties layoutPreservingProperties = new LayoutPreservingProperties();
    layoutPreservingProperties.put(1, "Value");
    layoutPreservingProperties.put(42, "Value");
    layoutPreservingProperties.setRemoveComments(true);
    layoutPreservingProperties.put("Key", "Value");

    // Act and Assert
    assertEquals("Value", layoutPreservingProperties.remove("Key"));
  }

  /**
   * Method under test: {@link LayoutPreservingProperties#setProperty(String, String)}
   */
  @Test
  public void testSetProperty() throws NullPointerException {
    // Arrange, Act and Assert
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "42"));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("Key", "\\\\\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("\\\\", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("\\=", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\=",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("\\:", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\:",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("\\ ", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\ ",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("\\t", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\t",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\t\f\r\n\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("tfrn\\:=#!",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("=", "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("=", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("=", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("=",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("", "\\\\\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\\:"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\\ "));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\\t"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\t\f\r\n\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "Value"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "tfrn\\:=#!"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "java.lang.Integer"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", ""));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\", "="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\\\\\\\",
        "org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine"));
    assertNull((new LayoutPreservingProperties()).setProperty("java.lang.Integer", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("42", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("\\", "\\\\"));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\\\"));
    assertNull((new LayoutPreservingProperties())
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\\\"));
    assertNull((new LayoutPreservingProperties()).setProperty("java.lang.Integer", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("42", "\\="));
    assertNull((new LayoutPreservingProperties()).setProperty("\\", "\\="));
    assertNull(
        (new LayoutPreservingProperties()).setProperty("org.apache.tools.ant.util.LayoutPreservingProperties", "\\="));
    assertNull((new LayoutPreservingProperties())
        .setProperty("org.apache.tools.ant.util.LayoutPreservingProperties$LogicalLine", "\\="));
  }
}

