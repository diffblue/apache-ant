package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.junitlauncher.confined.ForkDefinition.ForkMode;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.CommandlineJava;
import org.apache.tools.ant.types.Environment;
import org.apache.tools.ant.types.Environment.Variable;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ForkDefinitionDiffblueTest {
  /**
   * Test ForkMode {@link ForkMode#getValues()}.
   * <p>
   * Method under test: {@link ForkMode#getValues()}
   */
  @Test
  public void testForkModeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"perTestClass"}, (new ForkMode()).getValues());
  }

  /**
   * Test ForkMode {@link ForkMode#ForkMode()}.
   * <p>
   * Method under test: {@link ForkMode#ForkMode()}
   */
  @Test
  public void testForkModeNewForkMode() {
    // Arrange and Act
    ForkMode actualForkMode = new ForkMode();

    // Assert
    assertNull(actualForkMode.getValue());
    assertEquals(-1, actualForkMode.getIndex());
  }

  /**
   * Test {@link ForkDefinition#ForkDefinition()}.
   * <p>
   * Method under test: default or parameterless constructor of {@link ForkDefinition}
   */
  @Test
  public void testNewForkDefinition() throws BuildException {
    // Arrange and Act
    ForkDefinition actualForkDefinition = new ForkDefinition();

    // Assert
    Environment env = actualForkDefinition.getEnv();
    assertNull(env.getVariables());
    assertNull(actualForkDefinition.getDir());
    assertNull(actualForkDefinition.getForkMode());
    assertEquals(-1L, actualForkDefinition.getTimeout());
    assertTrue(env.getVariablesVector().isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ForkDefinition#setDir(String)}
   *   <li>{@link ForkDefinition#setForkMode(ForkMode)}
   *   <li>{@link ForkDefinition#setIncludeAntRuntimeLibraries(boolean)}
   *   <li>{@link ForkDefinition#setIncludeJUnitPlatformLibraries(boolean)}
   *   <li>{@link ForkDefinition#setTimeout(long)}
   *   <li>{@link ForkDefinition#getDir()}
   *   <li>{@link ForkDefinition#getEnv()}
   *   <li>{@link ForkDefinition#getForkMode()}
   *   <li>{@link ForkDefinition#getTimeout()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange
    ForkDefinition forkDefinition = new ForkDefinition();

    // Act
    forkDefinition.setDir("Dir");
    ForkMode forkMode = new ForkMode();
    forkDefinition.setForkMode(forkMode);
    forkDefinition.setIncludeAntRuntimeLibraries(true);
    forkDefinition.setIncludeJUnitPlatformLibraries(true);
    forkDefinition.setTimeout(10L);
    String actualDir = forkDefinition.getDir();
    Environment actualEnv = forkDefinition.getEnv();
    ForkMode actualForkMode = forkDefinition.getForkMode();
    long actualTimeout = forkDefinition.getTimeout();

    // Assert
    assertEquals("Dir", actualDir);
    assertNull(actualEnv.getVariables());
    assertEquals(10L, actualTimeout);
    assertSame(forkMode, actualForkMode);
  }

  /**
   * Test {@link ForkDefinition#createJvmArg()}.
   * <p>
   * Method under test: {@link ForkDefinition#createJvmArg()}
   */
  @Test
  public void testCreateJvmArg() {
    // Arrange and Act
    Argument actualCreateJvmArgResult = (new ForkDefinition()).createJvmArg();

    // Assert
    assertNull(actualCreateJvmArgResult.getParts());
    Location location = actualCreateJvmArgResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateJvmArgResult.getDescription());
    assertNull(actualCreateJvmArgResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link ForkDefinition#addConfiguredEnv(Variable)}.
   * <p>
   * Method under test: {@link ForkDefinition#addConfiguredEnv(Variable)}
   */
  @Test
  public void testAddConfiguredEnv() {
    // Arrange
    ForkDefinition forkDefinition = new ForkDefinition();
    Variable resultVar = new Variable();

    // Act
    forkDefinition.addConfiguredEnv(resultVar);

    // Assert
    Vector<Variable> variablesVector = forkDefinition.getEnv().getVariablesVector();
    assertEquals(1, variablesVector.size());
    assertSame(resultVar, variablesVector.get(0));
  }

  /**
   * Test {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}.
   * <p>
   * Method under test: {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}
   */
  @Test
  public void testGenerateCommandLine() {
    // Arrange
    ForkDefinition forkDefinition = new ForkDefinition();

    // Act
    CommandlineJava actualGenerateCommandLineResult = forkDefinition.generateCommandLine(new JUnitLauncherTask());

    // Assert
    String toStringResult = Paths.get(System.getProperty("java.home"), "bin", "java").toString();
    String toStringResult2 = Paths.get(System.getProperty("user.dir"), "build", "classes").toString();
    assertArrayEquals(new String[]{toStringResult, "-classpath", toStringResult2,
        String.join("", "org.apache.tools.ant.taskdefs.optional.junitlauncher.Standalone",
            System.getProperty("apple.awt.application.name"))},
        actualGenerateCommandLineResult.getCommandline());
  }

  /**
   * Test {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then return Classpath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}
   */
  @Test
  public void testGenerateCommandLine_givenProject_thenReturnClasspathProjectIsProject() {
    // Arrange
    ForkDefinition forkDefinition = new ForkDefinition();

    JUnitLauncherTask task = new JUnitLauncherTask();
    Project project = new Project();
    task.setProject(project);

    // Act and Assert
    assertSame(project, forkDefinition.generateCommandLine(task).getClasspath().getProject());
  }

  /**
   * Test {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}.
   * <ul>
   *   <li>Then return Modulepath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}
   */
  @Test
  public void testGenerateCommandLine_thenReturnModulepathDescriptionIsNull() {
    // Arrange
    ForkDefinition forkDefinition = new ForkDefinition();
    forkDefinition.addConfiguredModulePath(Path.systemBootClasspath);

    // Act and Assert
    Path modulepath = forkDefinition.generateCommandLine(new JUnitLauncherTask()).getModulepath();
    assertNull(modulepath.getDescription());
    assertNull(modulepath.getRefid());
    assertFalse(modulepath.isReference());
  }

  /**
   * Test {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}.
   * <ul>
   *   <li>Then return Upgrademodulepath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ForkDefinition#generateCommandLine(JUnitLauncherTask)}
   */
  @Test
  public void testGenerateCommandLine_thenReturnUpgrademodulepathDescriptionIsNull() {
    // Arrange
    ForkDefinition forkDefinition = new ForkDefinition();
    forkDefinition.addConfiguredUpgradeModulePath(Path.systemBootClasspath);

    // Act and Assert
    Path upgrademodulepath = forkDefinition.generateCommandLine(new JUnitLauncherTask()).getUpgrademodulepath();
    assertNull(upgrademodulepath.getDescription());
    assertNull(upgrademodulepath.getRefid());
    assertFalse(upgrademodulepath.isReference());
  }
}
