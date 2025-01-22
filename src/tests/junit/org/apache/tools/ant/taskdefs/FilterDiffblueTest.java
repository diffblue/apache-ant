package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class FilterDiffblueTest {
  /**
   * Test {@link Filter#execute()}.
   * <ul>
   *   <li>Given {@link Filter} (default constructor) Token is {@code ABC123}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Filter#execute()}
   */
  @Test
  public void testExecute_givenFilterTokenIsAbc123_thenThrowBuildException() throws BuildException {
    // Arrange
    Filter filter = new Filter();
    filter.setToken("ABC123");

    // Act and Assert
    assertThrows(BuildException.class, () -> filter.execute());
  }

  /**
   * Test {@link Filter#execute()}.
   * <ul>
   *   <li>Given {@link Filter} (default constructor) Token is {@code ABC123}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Filter#execute()}
   */
  @Test
  public void testExecute_givenFilterTokenIsAbc123_thenThrowBuildException2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Filter filter = new Filter();
    filter.setToken("ABC123");
    filter.setProject(project);
    filter.setFiltersfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> filter.execute());
  }

  /**
   * Test {@link Filter#execute()}.
   * <ul>
   *   <li>Given {@link Filter} (default constructor) Value is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Filter#execute()}
   */
  @Test
  public void testExecute_givenFilterValueIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Filter filter = new Filter();
    filter.setValue("42");
    filter.setProject(project);
    filter.setFiltersfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> filter.execute());
  }

  /**
   * Test {@link Filter#execute()}.
   * <ul>
   *   <li>Given {@link Filter} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Filter#execute()}
   */
  @Test
  public void testExecute_givenFilter_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Filter()).execute());
  }

  /**
   * Test new {@link Filter} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Filter}
   */
  @Test
  public void testNewFilter() {
    // Arrange and Act
    Filter actualFilter = new Filter();

    // Assert
    Location location = actualFilter.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFilter.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualFilter.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualFilter.getTaskName());
    assertNull(actualFilter.getTaskType());
    assertNull(actualFilter.getProject());
    assertNull(actualFilter.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualFilter, runtimeConfigurableWrapper.getProxy());
  }
}
