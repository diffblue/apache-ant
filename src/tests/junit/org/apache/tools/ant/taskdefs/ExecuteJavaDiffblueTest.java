package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Definer.OnError;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ExecuteJavaDiffblueTest {
  /**
   * Test {@link ExecuteJava#execute(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteJava#execute(Project)}
   */
  @Test
  public void testExecute_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    ExecuteJava executeJava = new ExecuteJava();
    executeJava.setClasspath(Path.systemBootClasspath);
    executeJava.setJavaCommand(new Commandline(null));

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> executeJava.execute(project));
  }

  /**
   * Test {@link ExecuteJava#execute(Project)}.
   * <ul>
   *   <li>Given {@link ExecuteJava} (default constructor) JavaCommand is {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteJava#execute(Project)}
   */
  @Test
  public void testExecute_givenExecuteJavaJavaCommandIsCommandlineWithToProcess() throws BuildException {
    // Arrange
    ExecuteJava executeJava = new ExecuteJava();
    executeJava.setJavaCommand(new Commandline("To Process"));

    // Act and Assert
    assertThrows(BuildException.class, () -> executeJava.execute(new Project()));
  }

  /**
   * Test {@link ExecuteJava#execute(Project)}.
   * <ul>
   *   <li>Given {@link OnError#POLICY_IGNORE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteJava#execute(Project)}
   */
  @Test
  public void testExecute_givenPolicy_ignore() throws BuildException {
    // Arrange
    ExecuteJava executeJava = new ExecuteJava();
    executeJava.setClasspath(Path.systemBootClasspath);
    executeJava.setJavaCommand(new Commandline(null));

    Project project = new Project();
    project.addOrReplaceTarget(OnError.POLICY_IGNORE, new Target());

    // Act and Assert
    assertThrows(BuildException.class, () -> executeJava.execute(project));
  }

  /**
   * Test {@link ExecuteJava#execute(Project)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteJava#execute(Project)}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    ExecuteJava executeJava = new ExecuteJava();
    executeJava.setClasspath(Path.systemBootClasspath);
    executeJava.setJavaCommand(new Commandline(null));

    // Act and Assert
    assertThrows(BuildException.class, () -> executeJava.execute(new Project()));
  }

  /**
   * Test {@link ExecuteJava#killedProcess()}.
   * <p>
   * Method under test: {@link ExecuteJava#killedProcess()}
   */
  @Test
  public void testKilledProcess() {
    // Arrange, Act and Assert
    assertFalse((new ExecuteJava()).killedProcess());
  }
}
