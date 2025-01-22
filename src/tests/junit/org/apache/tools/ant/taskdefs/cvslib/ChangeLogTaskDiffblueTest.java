package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ChangeLogTaskDiffblueTest {
  /**
   * Test {@link ChangeLogTask#execute()}.
   * <p>
   * Method under test: {@link ChangeLogTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    ChangeLogTask changeLogTask = new ChangeLogTask();
    changeLogTask.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> changeLogTask.execute());
  }

  /**
   * Test {@link ChangeLogTask#execute()}.
   * <p>
   * Method under test: {@link ChangeLogTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    project.addBuildListener(new AntClassLoader());

    ChangeLogTask changeLogTask = new ChangeLogTask();
    changeLogTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> changeLogTask.execute());
  }

  /**
   * Test {@link ChangeLogTask#execute()}.
   * <ul>
   *   <li>Given {@link ChangeLogTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogTask#execute()}
   */
  @Test
  public void testExecute_givenChangeLogTaskProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    ChangeLogTask changeLogTask = new ChangeLogTask();
    changeLogTask.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> changeLogTask.execute());
  }

  /**
   * Test {@link ChangeLogTask#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogTask#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Users", typeClass);
    project.addBuildListener(new AntClassLoader());

    ChangeLogTask changeLogTask = new ChangeLogTask();
    changeLogTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> changeLogTask.execute());
  }

  /**
   * Test {@link ChangeLogTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ChangeLogTask changeLogTask = new ChangeLogTask();
    changeLogTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> changeLogTask.execute());
  }

  /**
   * Test {@link ChangeLogTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    ChangeLogTask changeLogTask = new ChangeLogTask();
    changeLogTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> changeLogTask.execute());
  }

  /**
   * Test new {@link ChangeLogTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ChangeLogTask}
   */
  @Test
  public void testNewChangeLogTask() {
    // Arrange and Act
    ChangeLogTask actualChangeLogTask = new ChangeLogTask();

    // Assert
    assertNull(actualChangeLogTask.getDest());
    assertNull(actualChangeLogTask.getPassFile());
    assertNull(actualChangeLogTask.getDescription());
    assertNull(actualChangeLogTask.getTaskName());
    assertNull(actualChangeLogTask.getTaskType());
    assertNull(actualChangeLogTask.getCommand());
    assertNull(actualChangeLogTask.getCvsRoot());
    assertNull(actualChangeLogTask.getCvsRsh());
    assertNull(actualChangeLogTask.getPackage());
    assertNull(actualChangeLogTask.getTag());
    assertNull(actualChangeLogTask.getProject());
    assertNull(actualChangeLogTask.getOwningTarget());
    assertEquals(0, actualChangeLogTask.getPort());
  }
}
