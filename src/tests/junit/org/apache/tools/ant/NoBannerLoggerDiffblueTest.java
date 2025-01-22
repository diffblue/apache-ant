package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.junit.Test;

public class NoBannerLoggerDiffblueTest {
  /**
   * Test new {@link NoBannerLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link NoBannerLogger}
   */
  @Test
  public void testNewNoBannerLogger() {
    // Arrange and Act
    NoBannerLogger actualNoBannerLogger = new NoBannerLogger();

    // Assert
    assertEquals("BUILD FAILED", actualNoBannerLogger.getBuildFailedMessage());
    assertEquals("BUILD SUCCESSFUL", actualNoBannerLogger.getBuildSuccessfulMessage());
    assertNull(actualNoBannerLogger.err);
    assertNull(actualNoBannerLogger.out);
    assertNull(actualNoBannerLogger.targetName);
    assertEquals(0, actualNoBannerLogger.getMessageOutputLevel());
    assertFalse(actualNoBannerLogger.emacsMode);
  }

  /**
   * Test {@link NoBannerLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>Given {@link BigProjectLogger} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoBannerLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_givenBigProjectLogger_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(((NoBannerLogger) new BigProjectLogger()).extractTargetName(new BuildEvent(new Target())));
  }

  /**
   * Test {@link NoBannerLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>Given {@link NoBannerLogger} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoBannerLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_givenNoBannerLogger_thenReturnNull() {
    // Arrange
    NoBannerLogger noBannerLogger = new NoBannerLogger();

    // Act and Assert
    assertNull(noBannerLogger.extractTargetName(new BuildEvent(new Target())));
  }

  /**
   * Test {@link NoBannerLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Target#Target()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoBannerLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_givenProject_whenTargetProjectIsProject_thenReturnNull() {
    // Arrange
    Target target = new Target();
    target.setProject(new Project());

    // Act and Assert
    assertNull(((NoBannerLogger) new BigProjectLogger()).extractTargetName(new BuildEvent(target)));
  }
}
