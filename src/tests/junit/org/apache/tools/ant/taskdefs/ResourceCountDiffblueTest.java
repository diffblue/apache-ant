package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Ant.Reference;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class ResourceCountDiffblueTest {
  /**
   * Test {@link ResourceCount#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenResourceCountAddSystemBootClasspath_thenThrowBuildException() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> resourceCount.add(Path.systemBootClasspath));
  }

  /**
   * Test {@link ResourceCount#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@code 42} and {@code Value}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#setRefid(org.apache.tools.ant.types.Reference)}
   */
  @Test
  public void testSetRefid_givenProjectAddReference42AndValue_thenThrowBuildException() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();

    Project p = new Project();
    p.addReference("42", "Value");
    p.addBuildListener(new AntClassLoader());

    Reference r = new Reference();
    r.setProject(p);
    r.setRefId("42");
    r.setToRefid("No project set on reference to ");

    // Act and Assert
    assertThrows(BuildException.class, () -> resourceCount.setRefid(r));
  }

  /**
   * Test {@link ResourceCount#execute()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#execute()}
   */
  @Test
  public void testExecute_givenResourceCount_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ResourceCount()).execute());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.setCount(3);
    resourceCount
        .add(new Path(new Project(), "Use of the ResourceCount condition requires that the count attribute be set."));

    // Act and Assert
    assertFalse(resourceCount.eval());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval_givenResourceCountAddNone_thenReturnFalse() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.setCount(3);
    resourceCount.add(Resources.NONE);

    // Act and Assert
    assertFalse(resourceCount.eval());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor) add {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval_givenResourceCountAddPathWithProjectIsProject_thenReturnFalse() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.setCount(3);
    resourceCount.add(new Path(new Project()));

    // Act and Assert
    assertFalse(resourceCount.eval());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval_givenResourceCountAddSystemBootClasspath_thenThrowBuildException() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> resourceCount.eval());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor) Count is minus one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval_givenResourceCountCountIsMinusOne_thenReturnFalse() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.setCount(-1);
    resourceCount.add(Resources.NONE);

    // Act and Assert
    assertFalse(resourceCount.eval());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor) Count is zero.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval_givenResourceCountCountIsZero_thenReturnTrue() {
    // Arrange
    ResourceCount resourceCount = new ResourceCount();
    resourceCount.setCount(0);
    resourceCount.add(Resources.NONE);

    // Act and Assert
    assertTrue(resourceCount.eval());
  }

  /**
   * Test {@link ResourceCount#eval()}.
   * <ul>
   *   <li>Given {@link ResourceCount} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCount#eval()}
   */
  @Test
  public void testEval_givenResourceCount_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ResourceCount()).eval());
  }

  /**
   * Test new {@link ResourceCount} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ResourceCount}
   */
  @Test
  public void testNewResourceCount() {
    // Arrange and Act
    ResourceCount actualResourceCount = new ResourceCount();

    // Assert
    Location location = actualResourceCount.getLocation();
    assertNull(location.getFileName());
    assertNull(actualResourceCount.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualResourceCount.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualResourceCount.getTaskName());
    assertNull(actualResourceCount.getTaskType());
    assertNull(actualResourceCount.getProject());
    assertNull(actualResourceCount.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualResourceCount, runtimeConfigurableWrapper.getProxy());
  }
}
