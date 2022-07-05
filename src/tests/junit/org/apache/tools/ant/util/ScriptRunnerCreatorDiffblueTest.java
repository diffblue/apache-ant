package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import junit.runner.TestCaseClassLoader;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ScriptRunnerCreatorDiffblueTest {
  /**
  * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
  */
  @Test
  public void testCreateRunner() {
    // Arrange
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunnerCreator.createRunner((ScriptManager) null, null, new TestCaseClassLoader()));
  }

  /**
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunner2() {
    // Arrange
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunnerCreator.createRunner((ScriptManager) null, "en", new TestCaseClassLoader()));
  }

  /**
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunner3() {
    // Arrange
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunnerCreator.createRunner(ScriptManager.bsf, "en", new TestCaseClassLoader()));
  }

  /**
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunner4() {
    // Arrange
    Project project = new Project();
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(project);
    AntClassLoader antClassLoader = new AntClassLoader();

    // Act
    ScriptRunnerBase actualCreateRunnerResult = scriptRunnerCreator.createRunner(ScriptManager.auto, "en",
        antClassLoader);

    // Assert
    assertTrue(actualCreateRunnerResult.getBeans().isEmpty());
    assertSame(antClassLoader, actualCreateRunnerResult.getScriptClassLoader());
    assertEquals("", actualCreateRunnerResult.getScript());
    assertSame(project, actualCreateRunnerResult.getProject());
    assertEquals("en", actualCreateRunnerResult.getLanguage());
    assertFalse(actualCreateRunnerResult.getKeepEngine());
  }

  /**
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunner5() {
    // Arrange
    Project project = new Project();
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(project);
    AntClassLoader antClassLoader = new AntClassLoader();

    // Act
    ScriptRunnerBase actualCreateRunnerResult = scriptRunnerCreator.createRunner(ScriptManager.javax, "en",
        antClassLoader);

    // Assert
    assertTrue(actualCreateRunnerResult.getBeans().isEmpty());
    assertSame(antClassLoader, actualCreateRunnerResult.getScriptClassLoader());
    assertEquals("", actualCreateRunnerResult.getScript());
    assertSame(project, actualCreateRunnerResult.getProject());
    assertEquals("en", actualCreateRunnerResult.getLanguage());
    assertFalse(actualCreateRunnerResult.getKeepEngine());
  }
}

