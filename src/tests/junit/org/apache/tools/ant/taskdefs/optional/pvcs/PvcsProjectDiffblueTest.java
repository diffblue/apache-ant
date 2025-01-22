package org.apache.tools.ant.taskdefs.optional.pvcs;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class PvcsProjectDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link PvcsProject}
   *   <li>{@link PvcsProject#setName(String)}
   *   <li>{@link PvcsProject#getName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    PvcsProject actualPvcsProject = new PvcsProject();
    actualPvcsProject.setName("Name");

    // Assert
    assertEquals("Name", actualPvcsProject.getName());
  }
}
