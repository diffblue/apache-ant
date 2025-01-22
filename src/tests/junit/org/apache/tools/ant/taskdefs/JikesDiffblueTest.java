package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class JikesDiffblueTest {
  /**
   * Test {@link Jikes#Jikes(JikesOutputParser, String, Project)}.
   * <p>
   * Method under test: {@link Jikes#Jikes(JikesOutputParser, String, Project)}
   */
  @Test
  public void testNewJikes() {
    // Arrange
    JikesOutputParser jop = new JikesOutputParser(new TaskAdapter(), true);

    // Act and Assert
    assertEquals("Command", (new Jikes(jop, "Command", new Project())).command);
  }
}
