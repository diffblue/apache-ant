package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class DefaultLoggerDiffblueTest {
  /**
   * Test new {@link DefaultLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DefaultLogger}
   */
  @Test
  public void testNewDefaultLogger() {
    // Arrange and Act
    DefaultLogger actualDefaultLogger = new DefaultLogger();

    // Assert
    assertEquals("BUILD FAILED", actualDefaultLogger.getBuildFailedMessage());
    assertEquals("BUILD SUCCESSFUL", actualDefaultLogger.getBuildSuccessfulMessage());
    assertNull(actualDefaultLogger.err);
    assertNull(actualDefaultLogger.out);
    assertEquals(0, actualDefaultLogger.getMessageOutputLevel());
    assertFalse(actualDefaultLogger.emacsMode);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DefaultLogger#setEmacsMode(boolean)}
   *   <li>{@link DefaultLogger#setMessageOutputLevel(int)}
   *   <li>{@link DefaultLogger#log(String)}
   *   <li>{@link DefaultLogger#targetFinished(BuildEvent)}
   *   <li>{@link DefaultLogger#taskFinished(BuildEvent)}
   *   <li>{@link DefaultLogger#taskStarted(BuildEvent)}
   *   <li>{@link DefaultLogger#getBuildFailedMessage()}
   *   <li>{@link DefaultLogger#getBuildSuccessfulMessage()}
   *   <li>{@link DefaultLogger#getMessageOutputLevel()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    DefaultLogger defaultLogger = new DefaultLogger();

    // Act
    defaultLogger.setEmacsMode(true);
    defaultLogger.setMessageOutputLevel(1);
    defaultLogger.log("Not all who wander are lost");
    defaultLogger.targetFinished(new BuildEvent(new Project()));
    defaultLogger.taskFinished(new BuildEvent(new Project()));
    defaultLogger.taskStarted(new BuildEvent(new Project()));
    String actualBuildFailedMessage = defaultLogger.getBuildFailedMessage();
    String actualBuildSuccessfulMessage = defaultLogger.getBuildSuccessfulMessage();

    // Assert
    assertEquals("BUILD FAILED", actualBuildFailedMessage);
    assertEquals("BUILD SUCCESSFUL", actualBuildSuccessfulMessage);
    assertEquals(1, defaultLogger.getMessageOutputLevel());
  }

  /**
   * Test {@link DefaultLogger#formatTime(long)}.
   * <p>
   * Method under test: {@link DefaultLogger#formatTime(long)}
   */
  @Test
  public void testFormatTime() {
    // Arrange, Act and Assert
    assertEquals("0 seconds", DefaultLogger.formatTime(1L));
  }

  /**
   * Test {@link DefaultLogger#extractProjectName(BuildEvent)}.
   * <ul>
   *   <li>When {@link BuildEvent#BuildEvent(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultLogger#extractProjectName(BuildEvent)}
   */
  @Test
  public void testExtractProjectName_whenBuildEventWithProjectIsProject_thenReturnNull() {
    // Arrange
    DefaultLogger defaultLogger = new DefaultLogger();

    // Act and Assert
    assertNull(defaultLogger.extractProjectName(new BuildEvent(new Project())));
  }

  /**
   * Test {@link DefaultLogger#extractProjectName(BuildEvent)}.
   * <ul>
   *   <li>When {@link BuildEvent#BuildEvent(Target)} with target is {@link Target#Target()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultLogger#extractProjectName(BuildEvent)}
   */
  @Test
  public void testExtractProjectName_whenBuildEventWithTargetIsTarget_thenReturnNull() {
    // Arrange
    DefaultLogger defaultLogger = new DefaultLogger();

    // Act and Assert
    assertNull(defaultLogger.extractProjectName(new BuildEvent(new Target())));
  }
}
