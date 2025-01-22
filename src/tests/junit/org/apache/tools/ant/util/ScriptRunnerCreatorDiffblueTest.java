package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.util.optional.JavaxScriptRunner;
import org.junit.Test;

public class ScriptRunnerCreatorDiffblueTest {
  /**
   * Test {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)} with {@code ScriptManager}, {@code String}, {@code ClassLoader}.
   * <ul>
   *   <li>Then return {@link JavaxScriptRunner}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunnerWithScriptManagerStringClassLoader_thenReturnJavaxScriptRunner() {
    // Arrange
    Project project = new Project();
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(project);

    // Act
    ScriptRunnerBase actualCreateRunnerResult = scriptRunnerCreator.createRunner(ScriptManager.auto, "en",
        new AntClassLoader());

    // Assert
    assertTrue(actualCreateRunnerResult instanceof JavaxScriptRunner);
    assertEquals("", actualCreateRunnerResult.getScript());
    assertEquals("en", actualCreateRunnerResult.getLanguage());
    assertEquals("javax", actualCreateRunnerResult.getManagerName());
    assertNotNull(actualCreateRunnerResult.getScriptClassLoader());
    assertFalse(actualCreateRunnerResult.getCompiled());
    assertFalse(actualCreateRunnerResult.getKeepEngine());
    assertTrue(actualCreateRunnerResult.getBeans().isEmpty());
    assertSame(project, actualCreateRunnerResult.getProject());
  }

  /**
   * Test {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)} with {@code ScriptManager}, {@code String}, {@code ClassLoader}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunnerWithScriptManagerStringClassLoader_thenThrowBuildException() {
    // Arrange
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunnerCreator.createRunner((ScriptManager) null, null, new AntClassLoader()));
  }

  /**
   * Test {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)} with {@code ScriptManager}, {@code String}, {@code ClassLoader}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunnerWithScriptManagerStringClassLoader_thenThrowBuildException2() {
    // Arrange
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunnerCreator.createRunner((ScriptManager) null, "en", new AntClassLoader()));
  }

  /**
   * Test {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)} with {@code ScriptManager}, {@code String}, {@code ClassLoader}.
   * <ul>
   *   <li>When {@code bsf}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptRunnerCreator#createRunner(ScriptManager, String, ClassLoader)}
   */
  @Test
  public void testCreateRunnerWithScriptManagerStringClassLoader_whenBsf() {
    // Arrange
    ScriptRunnerCreator scriptRunnerCreator = new ScriptRunnerCreator(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> scriptRunnerCreator.createRunner(ScriptManager.bsf, "en", new AntClassLoader()));
  }
}
