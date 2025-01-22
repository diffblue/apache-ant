package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class IPlanetEjbcTaskDiffblueTest {
  /**
   * Test {@link IPlanetEjbcTask#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath} Project is {@code null}.</li>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbcTask#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenSystemBootClasspathProjectIsNull_whenPathWithProjectIsNull() {
    // Arrange
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    IPlanetEjbcTask iPlanetEjbcTask = new IPlanetEjbcTask();
    iPlanetEjbcTask.setClasspath(classpath);

    // Act
    iPlanetEjbcTask.setClasspath(new Path(null));

    // Assert that nothing has changed
    assertTrue(iPlanetEjbcTask.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link IPlanetEjbcTask#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbcTask#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull() {
    // Arrange
    IPlanetEjbcTask iPlanetEjbcTask = new IPlanetEjbcTask();
    iPlanetEjbcTask.setClasspath(Path.systemBootClasspath);

    // Act
    iPlanetEjbcTask.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(iPlanetEjbcTask.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link IPlanetEjbcTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link IPlanetEjbcTask} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbcTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenIPlanetEjbcTask_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new IPlanetEjbcTask()).createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link IPlanetEjbcTask#createClasspath()}.
   * <ul>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbcTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnProjectIsProject() {
    // Arrange
    IPlanetEjbcTask iPlanetEjbcTask = new IPlanetEjbcTask();
    Project project = new Project();
    iPlanetEjbcTask.setProject(project);

    // Act and Assert
    assertSame(project, iPlanetEjbcTask.createClasspath().getProject());
  }

  /**
   * Test {@link IPlanetEjbcTask#execute()}.
   * <p>
   * Method under test: {@link IPlanetEjbcTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    IPlanetEjbcTask iPlanetEjbcTask = new IPlanetEjbcTask();
    iPlanetEjbcTask.setClasspath(null);
    iPlanetEjbcTask.setEjbdescriptor(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    iPlanetEjbcTask.setIasdescriptor(null);
    iPlanetEjbcTask.setDest(null);
    iPlanetEjbcTask.setIashome(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> iPlanetEjbcTask.execute());
  }

  /**
   * Test {@link IPlanetEjbcTask#execute()}.
   * <p>
   * Method under test: {@link IPlanetEjbcTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    IPlanetEjbcTask iPlanetEjbcTask = new IPlanetEjbcTask();
    iPlanetEjbcTask.setClasspath(null);
    iPlanetEjbcTask
        .setEjbdescriptor(Paths.get(System.getProperty("java.io.tmpdir"), "The standard EJB descriptor (").toFile());
    iPlanetEjbcTask.setIasdescriptor(null);
    iPlanetEjbcTask.setDest(null);
    iPlanetEjbcTask.setIashome(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> iPlanetEjbcTask.execute());
  }

  /**
   * Test {@link IPlanetEjbcTask#execute()}.
   * <ul>
   *   <li>Given {@link IPlanetEjbcTask} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IPlanetEjbcTask#execute()}
   */
  @Test
  public void testExecute_givenIPlanetEjbcTask() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IPlanetEjbcTask()).execute());
  }

  /**
   * Test new {@link IPlanetEjbcTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IPlanetEjbcTask}
   */
  @Test
  public void testNewIPlanetEjbcTask() {
    // Arrange and Act
    IPlanetEjbcTask actualIPlanetEjbcTask = new IPlanetEjbcTask();

    // Assert
    Location location = actualIPlanetEjbcTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIPlanetEjbcTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualIPlanetEjbcTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualIPlanetEjbcTask.getTaskName());
    assertNull(actualIPlanetEjbcTask.getTaskType());
    assertNull(actualIPlanetEjbcTask.getProject());
    assertNull(actualIPlanetEjbcTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualIPlanetEjbcTask, runtimeConfigurableWrapper.getProxy());
  }
}
