package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class TruncateDiffblueTest {
  /**
   * Test {@link Truncate#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenTruncateAddSystemBootClasspath_whenSystemBootClasspath() {
    // Arrange
    Truncate truncate = new Truncate();
    truncate.add(Path.systemBootClasspath);

    // Act
    truncate.add(Path.systemBootClasspath);

    // Assert that nothing has changed
    assertNull(truncate.getProject());
  }

  /**
   * Test {@link Truncate#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor).</li>
   *   <li>Then {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenTruncateProjectIsProject_whenConcat_thenConcatProjectIsProject() {
    // Arrange
    Truncate truncate = new Truncate();
    Project project = new Project();
    truncate.setProject(project);
    Concat rc = new Concat();

    // Act
    truncate.add(rc);

    // Assert
    assertSame(project, rc.getProject());
    assertSame(project, truncate.getProject());
  }

  /**
   * Test {@link Truncate#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link Truncate} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenTruncateProjectIsProject_whenNone_thenTruncateProjectIsProject() {
    // Arrange
    Truncate truncate = new Truncate();
    Project project = new Project();
    truncate.setProject(project);

    // Act
    truncate.add(Resources.NONE);

    // Assert that nothing has changed
    assertSame(project, truncate.getProject());
  }

  /**
   * Test {@link Truncate#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link Truncate} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenTruncate_whenNone_thenTruncateProjectIsNull() {
    // Arrange
    Truncate truncate = new Truncate();

    // Act
    truncate.add(Resources.NONE);

    // Assert that nothing has changed
    assertNull(truncate.getProject());
  }

  /**
   * Test {@link Truncate#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Truncate} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenTruncate_whenNull_thenTruncateProjectIsNull() {
    // Arrange
    Truncate truncate = new Truncate();

    // Act
    truncate.add(null);

    // Assert that nothing has changed
    assertNull(truncate.getProject());
  }

  /**
   * Test {@link Truncate#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor).</li>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   *   <li>Then {@link Truncate} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenTruncate_whenSystemBootClasspath_thenTruncateProjectIsNull() {
    // Arrange
    Truncate truncate = new Truncate();

    // Act
    truncate.add(Path.systemBootClasspath);

    // Assert that nothing has changed
    assertNull(truncate.getProject());
  }

  /**
   * Test {@link Truncate#setLength(Long)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#setLength(Long)}
   */
  @Test
  public void testSetLength_whenMinusOne_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Truncate()).setLength(-1L));
  }

  /**
   * Test {@link Truncate#execute()}.
   * <ul>
   *   <li>Given {@link Truncate} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Truncate#execute()}
   */
  @Test
  public void testExecute_givenTruncate_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Truncate()).execute());
  }

  /**
   * Test new {@link Truncate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Truncate}
   */
  @Test
  public void testNewTruncate() {
    // Arrange and Act
    Truncate actualTruncate = new Truncate();

    // Assert
    Location location = actualTruncate.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTruncate.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualTruncate.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualTruncate.getTaskName());
    assertNull(actualTruncate.getTaskType());
    assertNull(actualTruncate.getProject());
    assertNull(actualTruncate.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualTruncate, runtimeConfigurableWrapper.getProxy());
  }
}
