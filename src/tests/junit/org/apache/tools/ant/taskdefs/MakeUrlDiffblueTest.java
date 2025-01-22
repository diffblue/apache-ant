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
import org.junit.Test;

public class MakeUrlDiffblueTest {
  /**
   * Test {@link MakeUrl#execute()}.
   * <ul>
   *   <li>Given {@link MakeUrl} (default constructor) addPath {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MakeUrl#execute()}
   */
  @Test
  public void testExecute_givenMakeUrlAddPathPathWithPIsProjectAndPath_thenThrowBuildException() throws BuildException {
    // Arrange
    MakeUrl makeUrl = new MakeUrl();
    makeUrl.setProject(new Project());
    makeUrl.addPath(new Path(new Project(), "Path"));
    makeUrl.setProperty("https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> makeUrl.execute());
  }

  /**
   * Test {@link MakeUrl#execute()}.
   * <ul>
   *   <li>Given {@link MakeUrl} (default constructor) Property is {@code https://example.org/example}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MakeUrl#execute()}
   */
  @Test
  public void testExecute_givenMakeUrlPropertyIsHttpsExampleOrgExample_thenThrowBuildException() throws BuildException {
    // Arrange
    MakeUrl makeUrl = new MakeUrl();
    makeUrl.setProperty("https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> makeUrl.execute());
  }

  /**
   * Test {@link MakeUrl#execute()}.
   * <ul>
   *   <li>Given {@link MakeUrl} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MakeUrl#execute()}
   */
  @Test
  public void testExecute_givenMakeUrl_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MakeUrl()).execute());
  }

  /**
   * Test new {@link MakeUrl} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MakeUrl}
   */
  @Test
  public void testNewMakeUrl() {
    // Arrange and Act
    MakeUrl actualMakeUrl = new MakeUrl();

    // Assert
    Location location = actualMakeUrl.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMakeUrl.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualMakeUrl.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualMakeUrl.getTaskName());
    assertNull(actualMakeUrl.getTaskType());
    assertNull(actualMakeUrl.getProject());
    assertNull(actualMakeUrl.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualMakeUrl, runtimeConfigurableWrapper.getProxy());
  }
}
