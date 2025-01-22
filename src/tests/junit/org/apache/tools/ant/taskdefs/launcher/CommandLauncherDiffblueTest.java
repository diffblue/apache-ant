package org.apache.tools.ant.taskdefs.launcher;

import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class CommandLauncherDiffblueTest {
  /**
   * Test {@link CommandLauncher#exec(Project, String[], String[], File)} with {@code project}, {@code cmd}, {@code env}, {@code workingDir}.
   * <ul>
   *   <li>Given {@link CommandLauncher} (default constructor).</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#exec(Project, String[], String[], File)}
   */
  @Test
  public void testExecWithProjectCmdEnvWorkingDir_givenCommandLauncher_thenThrowIOException() throws IOException {
    // Arrange
    CommandLauncher commandLauncher = new CommandLauncher();
    Project project = new Project();

    // Act and Assert
    assertThrows(IOException.class, () -> commandLauncher.exec(project, new String[]{"Cmd"}, new String[]{"Env"},
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link CommandLauncher#getShellLauncher(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getShellLauncher(Project)}
   */
  @Test
  public void testGetShellLauncher_givenAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getShellLauncher(project) instanceof ScriptCommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getShellLauncher(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getShellLauncher(Project)}
   */
  @Test
  public void testGetShellLauncher_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getShellLauncher(project) instanceof ScriptCommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getShellLauncher(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getShellLauncher(Project)}
   */
  @Test
  public void testGetShellLauncher_givenTarget_whenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getShellLauncher(project) instanceof ScriptCommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getShellLauncher(Project)}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   *   <li>When {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getShellLauncher(Project)}
   */
  @Test
  public void testGetShellLauncher_givenValue_whenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getShellLauncher(project) instanceof ScriptCommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getShellLauncher(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getShellLauncher(Project)}
   */
  @Test
  public void testGetShellLauncher_whenNull() {
    // Arrange, Act and Assert
    assertTrue(CommandLauncher.getShellLauncher(null) instanceof ScriptCommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getShellLauncher(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getShellLauncher(Project)}
   */
  @Test
  public void testGetShellLauncher_whenProject() {
    // Arrange, Act and Assert
    assertTrue(CommandLauncher.getShellLauncher(new Project()) instanceof ScriptCommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getVMLauncher(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getVMLauncher(Project)}
   */
  @Test
  public void testGetVMLauncher_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getVMLauncher(project) instanceof Java13CommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getVMLauncher(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getVMLauncher(Project)}
   */
  @Test
  public void testGetVMLauncher_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getVMLauncher(project) instanceof Java13CommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getVMLauncher(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getVMLauncher(Project)}
   */
  @Test
  public void testGetVMLauncher_givenTarget_whenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getVMLauncher(project) instanceof Java13CommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getVMLauncher(Project)}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   *   <li>When {@link Project} (default constructor) addReference {@code ant.PropertyHelper} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getVMLauncher(Project)}
   */
  @Test
  public void testGetVMLauncher_givenValue_whenProjectAddReferenceAntPropertyHelperAndValue() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(CommandLauncher.getVMLauncher(project) instanceof Java13CommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getVMLauncher(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getVMLauncher(Project)}
   */
  @Test
  public void testGetVMLauncher_whenNull() {
    // Arrange, Act and Assert
    assertTrue(CommandLauncher.getVMLauncher(null) instanceof Java13CommandLauncher);
  }

  /**
   * Test {@link CommandLauncher#getVMLauncher(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncher#getVMLauncher(Project)}
   */
  @Test
  public void testGetVMLauncher_whenProject() {
    // Arrange, Act and Assert
    assertTrue(CommandLauncher.getVMLauncher(new Project()) instanceof Java13CommandLauncher);
  }
}
