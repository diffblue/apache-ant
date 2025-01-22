package org.apache.tools.ant.taskdefs.launcher;

import static org.junit.Assert.assertThrows;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class PerlScriptCommandLauncherDiffblueTest {
  /**
   * Test {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)} with {@code project}, {@code cmd}, {@code env}, {@code workingDir}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)}
   */
  @Test
  public void testExecWithProjectCmdEnvWorkingDir_givenAntClassLoader() throws IOException {
    // Arrange
    PerlScriptCommandLauncher perlScriptCommandLauncher = new PerlScriptCommandLauncher("Script",
        new CommandLauncher());

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(IOException.class, () -> perlScriptCommandLauncher.exec(project, new String[]{"Cmd"},
        new String[]{"Env"}, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)} with {@code project}, {@code cmd}, {@code env}, {@code workingDir}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)}
   */
  @Test
  public void testExecWithProjectCmdEnvWorkingDir_givenJavaLangObject() throws IOException {
    // Arrange
    PerlScriptCommandLauncher perlScriptCommandLauncher = new PerlScriptCommandLauncher("Script",
        new CommandLauncher());

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(IOException.class, () -> perlScriptCommandLauncher.exec(project, new String[]{"Cmd"},
        new String[]{"Env"}, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)} with {@code project}, {@code cmd}, {@code env}, {@code workingDir}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)}
   */
  @Test
  public void testExecWithProjectCmdEnvWorkingDir_givenTarget() throws IOException, BuildException {
    // Arrange
    PerlScriptCommandLauncher perlScriptCommandLauncher = new PerlScriptCommandLauncher("Script",
        new CommandLauncher());

    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(IOException.class, () -> perlScriptCommandLauncher.exec(project, new String[]{"Cmd"},
        new String[]{"Env"}, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)} with {@code project}, {@code cmd}, {@code env}, {@code workingDir}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)}
   */
  @Test
  public void testExecWithProjectCmdEnvWorkingDir_whenNull_thenThrowIOException() throws IOException {
    // Arrange
    PerlScriptCommandLauncher perlScriptCommandLauncher = new PerlScriptCommandLauncher("Script",
        new CommandLauncher());

    // Act and Assert
    assertThrows(IOException.class, () -> perlScriptCommandLauncher.exec(null, new String[]{"Cmd"}, new String[]{"Env"},
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)} with {@code project}, {@code cmd}, {@code env}, {@code workingDir}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PerlScriptCommandLauncher#exec(Project, String[], String[], File)}
   */
  @Test
  public void testExecWithProjectCmdEnvWorkingDir_whenProject_thenThrowIOException() throws IOException {
    // Arrange
    PerlScriptCommandLauncher perlScriptCommandLauncher = new PerlScriptCommandLauncher("Script",
        new CommandLauncher());
    Project project = new Project();

    // Act and Assert
    assertThrows(IOException.class, () -> perlScriptCommandLauncher.exec(project, new String[]{"Cmd"},
        new String[]{"Env"}, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
