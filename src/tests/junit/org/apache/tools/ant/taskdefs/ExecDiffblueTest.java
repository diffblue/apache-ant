package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class ExecDiffblueTest {
  /**
   * Test new {@link Exec} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Exec}
   */
  @Test
  public void testNewExec() {
    // Arrange and Act
    Exec actualExec = new Exec();

    // Assert
    assertNull(actualExec.fos);
    Location location = actualExec.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExec.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualExec.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualExec.getTaskName());
    assertNull(actualExec.getTaskType());
    assertNull(actualExec.getProject());
    assertNull(actualExec.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualExec, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link Exec#execute()}.
   * <ul>
   *   <li>Given {@link Exec} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#execute()}
   */
  @Test
  public void testExecute_givenExecProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Exec exec = new Exec();
    exec.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> exec.execute());
  }

  /**
   * Test {@link Exec#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Exec exec = new Exec();
    exec.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> exec.execute());
  }

  /**
   * Test {@link Exec#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Exec exec = new Exec();
    exec.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> exec.execute());
  }

  /**
   * Test {@link Exec#run(String)}.
   * <ul>
   *   <li>Given {@link Exec} (default constructor) Os is {@code os.name}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#run(String)}
   */
  @Test
  public void testRun_givenExecOsIsOsName_thenReturnZero() throws BuildException {
    // Arrange
    Exec exec = new Exec();
    exec.setOs("os.name");

    // Act and Assert
    assertEquals(0, exec.run("Command"));
  }

  /**
   * Test {@link Exec#run(String)}.
   * <ul>
   *   <li>Given {@link Exec} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#run(String)}
   */
  @Test
  public void testRun_givenExecProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Exec exec = new Exec();
    exec.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> exec.run("Command"));
  }

  /**
   * Test {@link Exec#run(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#run(String)}
   */
  @Test
  public void testRun_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Exec exec = new Exec();
    exec.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> exec.run("Command"));
  }

  /**
   * Test {@link Exec#run(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exec#run(String)}
   */
  @Test
  public void testRun_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Exec exec = new Exec();
    exec.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> exec.run("Command"));
  }
}
