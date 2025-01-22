package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class SimpleBigProjectLoggerDiffblueTest {
  /**
   * Test {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>When {@link Target#Target()} Name is {@code Name}.</li>
   *   <li>Then return {@code Name.Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_givenName_whenTargetNameIsName_thenReturnNameName() {
    // Arrange
    SimpleBigProjectLogger simpleBigProjectLogger = new SimpleBigProjectLogger();

    Project project = new Project();
    project.setName("Name");

    Target target = new Target();
    target.setName("Name");
    target.setProject(project);

    // Act and Assert
    assertEquals("Name.Name", simpleBigProjectLogger.extractTargetName(new BuildEvent(target)));
  }

  /**
   * Test {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_givenProjectNameIsName_thenReturnNull() {
    // Arrange
    SimpleBigProjectLogger simpleBigProjectLogger = new SimpleBigProjectLogger();

    Project project = new Project();
    project.setName("Name");

    Target target = new Target();
    target.setProject(project);

    // Act and Assert
    assertNull(simpleBigProjectLogger.extractTargetName(new BuildEvent(target)));
  }

  /**
   * Test {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Target#Target()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_givenProject_whenTargetProjectIsProject_thenReturnNull() {
    // Arrange
    SimpleBigProjectLogger simpleBigProjectLogger = new SimpleBigProjectLogger();

    Target target = new Target();
    target.setProject(new Project());

    // Act and Assert
    assertNull(simpleBigProjectLogger.extractTargetName(new BuildEvent(target)));
  }

  /**
   * Test {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}.
   * <ul>
   *   <li>When {@link BuildEvent#BuildEvent(Target)} with target is {@link Target#Target()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SimpleBigProjectLogger#extractTargetName(BuildEvent)}
   */
  @Test
  public void testExtractTargetName_whenBuildEventWithTargetIsTarget_thenReturnNull() {
    // Arrange
    SimpleBigProjectLogger simpleBigProjectLogger = new SimpleBigProjectLogger();

    // Act and Assert
    assertNull(simpleBigProjectLogger.extractTargetName(new BuildEvent(new Target())));
  }

  /**
   * Test new {@link SimpleBigProjectLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SimpleBigProjectLogger}
   */
  @Test
  public void testNewSimpleBigProjectLogger() {
    // Arrange, Act and Assert
    assertEquals(0, (new SimpleBigProjectLogger()).getMessageOutputLevel());
  }
}
