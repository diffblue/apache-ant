package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.junit.Test;

public class XmlLoggerDiffblueTest {
  /**
   * Test {@link XmlLogger#taskFinished(BuildEvent)}.
   * <ul>
   *   <li>When {@link BuildEvent#BuildEvent(Task)} with task is {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then throw {@link RuntimeException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XmlLogger#taskFinished(BuildEvent)}
   */
  @Test
  public void testTaskFinished_whenBuildEventWithTaskIsTaskAdapter_thenThrowRuntimeException() {
    // Arrange
    XmlLogger xmlLogger = new XmlLogger();

    // Act and Assert
    assertThrows(RuntimeException.class, () -> xmlLogger.taskFinished(new BuildEvent(new TaskAdapter())));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link XmlLogger#setMessageOutputLevel(int)}
   *   <li>{@link XmlLogger#setEmacsMode(boolean)}
   *   <li>{@link XmlLogger#setErrorPrintStream(PrintStream)}
   *   <li>{@link XmlLogger#getMessageOutputLevel()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    XmlLogger xmlLogger = new XmlLogger();

    // Act
    xmlLogger.setMessageOutputLevel(1);
    xmlLogger.setEmacsMode(true);
    xmlLogger.setErrorPrintStream(new PrintStream(new ByteArrayOutputStream(1)));

    // Assert
    assertEquals(1, xmlLogger.getMessageOutputLevel());
  }

  /**
   * Test new {@link XmlLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XmlLogger}
   */
  @Test
  public void testNewXmlLogger() {
    // Arrange, Act and Assert
    assertEquals(4, (new XmlLogger()).getMessageOutputLevel());
  }
}
