package org.apache.tools.ant.types.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ScriptFilterDiffblueTest {
  /**
   * Test {@link ScriptFilter#setProject(Project)}.
   * <p>
   * Method under test: {@link ScriptFilter#setProject(Project)}
   */
  @Test
  public void testSetProject() {
    // Arrange
    ScriptFilter scriptFilter = new ScriptFilter();
    Project project = new Project();

    // Act
    scriptFilter.setProject(project);

    // Assert
    assertSame(project, scriptFilter.getProject());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ScriptFilter#setToken(String)}
   *   <li>{@link ScriptFilter#getToken()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ScriptFilter scriptFilter = new ScriptFilter();

    // Act
    scriptFilter.setToken("ABC123");

    // Assert
    assertEquals("ABC123", scriptFilter.getToken());
  }

  /**
   * Test {@link ScriptFilter#createClasspath()}.
   * <ul>
   *   <li>Then return Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptFilter#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenReturnDescriptionIsNull() {
    // Arrange
    ScriptFilter scriptFilter = new ScriptFilter();
    Project project = new Project();
    scriptFilter.setProject(project);

    // Act
    Path actualCreateClasspathResult = scriptFilter.createClasspath();

    // Assert
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
    assertSame(project, actualCreateClasspathResult.getProject());
  }

  /**
   * Test new {@link ScriptFilter} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptFilter}
   */
  @Test
  public void testNewScriptFilter() {
    // Arrange and Act
    ScriptFilter actualScriptFilter = new ScriptFilter();

    // Assert
    Location location = actualScriptFilter.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScriptFilter.getDescription());
    assertNull(actualScriptFilter.getToken());
    assertNull(actualScriptFilter.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
