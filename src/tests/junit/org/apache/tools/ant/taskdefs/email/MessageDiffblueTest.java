package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class MessageDiffblueTest {
  /**
   * Test {@link Message#Message()}.
   * <p>
   * Method under test: {@link Message#Message()}
   */
  @Test
  public void testNewMessage() {
    // Arrange and Act
    Message actualMessage = new Message();

    // Assert
    assertEquals("text/plain", actualMessage.getMimeType());
    Location location = actualMessage.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMessage.getDescription());
    assertNull(actualMessage.getCharset());
    assertNull(actualMessage.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMessage.isMimeTypeSpecified());
  }

  /**
   * Test {@link Message#Message(File)}.
   * <p>
   * Method under test: {@link Message#Message(File)}
   */
  @Test
  public void testNewMessage2() {
    // Arrange and Act
    Message actualMessage = new Message(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals("text/plain", actualMessage.getMimeType());
    Location location = actualMessage.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMessage.getDescription());
    assertNull(actualMessage.getCharset());
    assertNull(actualMessage.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMessage.isMimeTypeSpecified());
  }

  /**
   * Test {@link Message#Message(String)}.
   * <p>
   * Method under test: {@link Message#Message(String)}
   */
  @Test
  public void testNewMessage3() {
    // Arrange and Act
    Message actualMessage = new Message("Text");

    // Assert
    assertEquals("text/plain", actualMessage.getMimeType());
    Location location = actualMessage.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMessage.getDescription());
    assertNull(actualMessage.getCharset());
    assertNull(actualMessage.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMessage.isMimeTypeSpecified());
  }

  /**
   * Test {@link Message#setMimeType(String)}.
   * <p>
   * Method under test: {@link Message#setMimeType(String)}
   */
  @Test
  public void testSetMimeType() {
    // Arrange
    Message message = new Message();

    // Act
    message.setMimeType("Mime Type");

    // Assert
    assertEquals("Mime Type", message.getMimeType());
    assertTrue(message.isMimeTypeSpecified());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Message#setCharset(String)}
   *   <li>{@link Message#setInputEncoding(String)}
   *   <li>{@link Message#setSrc(File)}
   *   <li>{@link Message#getCharset()}
   *   <li>{@link Message#getMimeType()}
   *   <li>{@link Message#isMimeTypeSpecified()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Message message = new Message();

    // Act
    message.setCharset("UTF-8");
    message.setInputEncoding("UTF-8");
    message.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    String actualCharset = message.getCharset();
    String actualMimeType = message.getMimeType();

    // Assert
    assertEquals("UTF-8", actualCharset);
    assertEquals("text/plain", actualMimeType);
    assertFalse(message.isMimeTypeSpecified());
  }
}
