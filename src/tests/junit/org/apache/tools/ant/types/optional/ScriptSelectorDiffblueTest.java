package org.apache.tools.ant.types.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ScriptSelectorDiffblueTest {
  /**
   * Test {@link ScriptSelector#setProject(Project)}.
   * <p>
   * Method under test: {@link ScriptSelector#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    Project project = new Project();

    // Act
    scriptSelector.setProject(project);

    // Assert
    assertSame(project, scriptSelector.getProject());
  }

  /**
   * Test {@link ScriptSelector#createClasspath()}.
   * <ul>
   *   <li>Then return Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptSelector#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnDescriptionIsNull() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    Project project = new Project();
    scriptSelector.setProject(project);

    // Act
    Path actualCreateClasspathResult = scriptSelector.createClasspath();

    // Assert
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
    assertSame(project, actualCreateClasspathResult.getProject());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ScriptSelector#setSelected(boolean)}
   *   <li>{@link ScriptSelector#getBasedir()}
   *   <li>{@link ScriptSelector#getFile()}
   *   <li>{@link ScriptSelector#getFilename()}
   *   <li>{@link ScriptSelector#isSelected()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();

    // Act
    scriptSelector.setSelected(true);
    File actualBasedir = scriptSelector.getBasedir();
    File actualFile = scriptSelector.getFile();
    String actualFilename = scriptSelector.getFilename();

    // Assert
    assertNull(actualBasedir);
    assertNull(actualFile);
    assertNull(actualFilename);
    assertTrue(scriptSelector.isSelected());
  }

  /**
   * Test new {@link ScriptSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptSelector}
   */
  @Test
  public void testNewScriptSelector() {
    // Arrange and Act
    ScriptSelector actualScriptSelector = new ScriptSelector();

    // Assert
    assertNull(actualScriptSelector.getBasedir());
    assertNull(actualScriptSelector.getFile());
    Location location = actualScriptSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScriptSelector.getDescription());
    assertNull(actualScriptSelector.getFilename());
    assertNull(actualScriptSelector.getError());
    assertNull(actualScriptSelector.getProject());
    assertNull(actualScriptSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualScriptSelector.isReference());
    assertFalse(actualScriptSelector.isSelected());
  }
}
