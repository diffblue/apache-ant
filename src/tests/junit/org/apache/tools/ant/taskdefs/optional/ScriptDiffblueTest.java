package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ScriptDiffblueTest {
  /**
   * Test {@link Script#setProject(Project)}.
   * <p>
   * Method under test: {@link Script#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    Script script = new Script();
    Project project = new Project();

    // Act
    script.setProject(project);

    // Assert
    assertSame(project, script.getProject());
  }

  /**
   * Test {@link Script#createClasspath()}.
   * <ul>
   *   <li>Given {@link Script} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Script#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenScriptProjectIsProject_thenReturnDescriptionIsNull() {
    // Arrange
    Script script = new Script();
    Project project = new Project();
    script.setProject(project);

    // Act
    Path actualCreateClasspathResult = script.createClasspath();

    // Assert
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
    assertSame(project, actualCreateClasspathResult.getProject());
  }

  /**
   * Test new {@link Script} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Script}
   */
  @Test
  public void testNewScript() {
    // Arrange and Act
    Script actualScript = new Script();

    // Assert
    Location location = actualScript.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScript.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualScript.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualScript.getTaskName());
    assertNull(actualScript.getTaskType());
    assertNull(actualScript.getProject());
    assertNull(actualScript.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualScript, runtimeConfigurableWrapper.getProxy());
  }
}
