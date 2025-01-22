package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.junit.Test;

public class MainDiffblueTest {
  /**
   * Test {@link Main#addBuildListeners(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Main#addBuildListeners(Project)}
   */
  @Test
  public void testAddBuildListeners_givenAntClassLoader_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    Main main = new Main();

    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    // Act
    main.addBuildListeners(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    BuildListener getResult = buildListeners.get(1);
    assertTrue(getResult instanceof DefaultLogger);
    assertEquals("BUILD FAILED", ((DefaultLogger) getResult).getBuildFailedMessage());
    assertEquals("BUILD SUCCESSFUL", ((DefaultLogger) getResult).getBuildSuccessfulMessage());
    assertEquals(2, ((DefaultLogger) getResult).getMessageOutputLevel());
    assertFalse(((DefaultLogger) getResult).emacsMode);
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link Main#addBuildListeners(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Main#addBuildListeners(Project)}
   */
  @Test
  public void testAddBuildListeners_whenProject_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    Main main = new Main();
    Project project = new Project();

    // Act
    main.addBuildListeners(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(1, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof DefaultLogger);
    assertEquals("BUILD FAILED", ((DefaultLogger) getResult).getBuildFailedMessage());
    assertEquals("BUILD SUCCESSFUL", ((DefaultLogger) getResult).getBuildSuccessfulMessage());
    assertEquals(2, ((DefaultLogger) getResult).getMessageOutputLevel());
    assertFalse(((DefaultLogger) getResult).emacsMode);
  }

  /**
   * Test {@link Main#getShortAntVersion()}.
   * <p>
   * Method under test: {@link Main#getShortAntVersion()}
   */
  @Test
  public void testGetShortAntVersion() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("1.10.15", Main.getShortAntVersion());
  }
}
