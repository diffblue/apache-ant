package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertSame;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ResourceContainsDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ResourceContains}
   *   <li>{@link ResourceContains#setCasesensitive(boolean)}
   *   <li>{@link ResourceContains#setProject(Project)}
   *   <li>{@link ResourceContains#setRefid(String)}
   *   <li>{@link ResourceContains#setSubstring(String)}
   *   <li>{@link ResourceContains#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ResourceContains actualResourceContains = new ResourceContains();
    actualResourceContains.setCasesensitive(true);
    Project project = new Project();
    actualResourceContains.setProject(project);
    actualResourceContains.setRefid("Refid");
    actualResourceContains.setSubstring("Substring");

    // Assert
    assertSame(project, actualResourceContains.getProject());
  }
}
