package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.ExecutorTest;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class FailureRecorderDiffblueTest {
  /**
   * Test {@link FailureRecorder#setProject(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailureRecorder#setProject(Project)}
   */
  @Test
  public void testSetProject_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    FailureRecorder failureRecorder = new FailureRecorder();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    failureRecorder.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(failureRecorder, buildListeners.get(1));
  }

  /**
   * Test {@link FailureRecorder#setProject(Project)}.
   * <ul>
   *   <li>Given {@code ) as BuildListener}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailureRecorder#setProject(Project)}
   */
  @Test
  public void testSetProject_givenAsBuildListener() {
    // Arrange
    FailureRecorder failureRecorder = new FailureRecorder();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(") as BuildListener", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act
    failureRecorder.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(failureRecorder, buildListeners.get(1));
  }

  /**
   * Test {@link FailureRecorder#setProject(Project)}.
   * <ul>
   *   <li>Given {@link DefaultLogger} (default constructor).</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FailureRecorder#setProject(Project)}
   */
  @Test
  public void testSetProject_givenDefaultLogger_whenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    FailureRecorder failureRecorder = new FailureRecorder();

    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act
    failureRecorder.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(failureRecorder, buildListeners.get(1));
  }

  /**
   * Test {@link FailureRecorder#setProject(Project)}.
   * <ul>
   *   <li>Given {@link ExecutorTest} (default constructor).</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FailureRecorder#setProject(Project)}
   */
  @Test
  public void testSetProject_givenExecutorTest_whenProjectAddBuildListenerExecutorTest() {
    // Arrange
    FailureRecorder failureRecorder = new FailureRecorder();

    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    // Act
    failureRecorder.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(failureRecorder, buildListeners.get(1));
  }

  /**
   * Test {@link FailureRecorder#setProject(Project)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners first {@link FailureRecorder}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailureRecorder#setProject(Project)}
   */
  @Test
  public void testSetProject_thenProjectBuildListenersFirstFailureRecorder() {
    // Arrange
    FailureRecorder failureRecorder = new FailureRecorder();

    Project project = new Project();
    FailureRecorder listener = new FailureRecorder();
    project.addBuildListener(listener);

    // Act
    failureRecorder.setProject(project);

    // Assert that nothing has changed
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(1, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof FailureRecorder);
    assertSame(listener, getResult);
  }

  /**
   * Test {@link FailureRecorder#setProject(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailureRecorder#setProject(Project)}
   */
  @Test
  public void testSetProject_whenProject_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    FailureRecorder failureRecorder = new FailureRecorder();
    Project project = new Project();

    // Act
    failureRecorder.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(failureRecorder, buildListeners.get(0));
  }

  /**
   * Test new {@link FailureRecorder} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FailureRecorder}
   */
  @Test
  public void testNewFailureRecorder() {
    // Arrange and Act
    FailureRecorder actualFailureRecorder = new FailureRecorder();

    // Assert
    Location location = actualFailureRecorder.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFailureRecorder.getDescription());
    assertNull(actualFailureRecorder.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
