package org.apache.tools.ant.types;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.junit.Test;

public class ResourceCollectionDiffblueTest {
  /**
   * Test {@link ResourceCollection#isEmpty()}.
   * <p>
   * Method under test: {@link ResourceCollection#isEmpty()}
   */
  @Test
  public void testIsEmpty() throws BuildException {
    // Arrange
    Path path = new Path(new Project());
    path.add(new Path(new Project(), "No directory specified for %s."));

    // Act and Assert
    assertFalse(path.isEmpty());
  }

  /**
   * Test {@link ResourceCollection#isEmpty()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCollection#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenConcat_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Concat()).isEmpty());
  }

  /**
   * Test {@link ResourceCollection#isEmpty()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCollection#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenPathWithPIsProjectAndPathIsNoDirectorySpecifiedForS() {
    // Arrange, Act and Assert
    assertFalse((new Path(new Project(), "No directory specified for %s.")).isEmpty());
  }

  /**
   * Test {@link ResourceCollection#isEmpty()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceCollection#isEmpty()}
   */
  @Test
  public void testIsEmpty_givenPathWithProjectIsProject_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Path(new Project())).isEmpty());
  }
}
