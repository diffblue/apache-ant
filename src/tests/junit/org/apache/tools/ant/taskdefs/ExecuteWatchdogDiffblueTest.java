package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertFalse;
import org.junit.Test;

public class ExecuteWatchdogDiffblueTest {
  /**
   * Test {@link ExecuteWatchdog#ExecuteWatchdog(int)}.
   * <ul>
   *   <li>When ten.</li>
   *   <li>Then return not Watching.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteWatchdog#ExecuteWatchdog(int)}
   */
  @Test
  public void testNewExecuteWatchdog_whenTen_thenReturnNotWatching() {
    // Arrange and Act
    ExecuteWatchdog actualExecuteWatchdog = new ExecuteWatchdog(10);

    // Assert
    assertFalse(actualExecuteWatchdog.isWatching());
    assertFalse(actualExecuteWatchdog.killedProcess());
  }

  /**
   * Test {@link ExecuteWatchdog#ExecuteWatchdog(long)}.
   * <ul>
   *   <li>When ten.</li>
   *   <li>Then return not Watching.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecuteWatchdog#ExecuteWatchdog(long)}
   */
  @Test
  public void testNewExecuteWatchdog_whenTen_thenReturnNotWatching2() {
    // Arrange and Act
    ExecuteWatchdog actualExecuteWatchdog = new ExecuteWatchdog(10L);

    // Assert
    assertFalse(actualExecuteWatchdog.isWatching());
    assertFalse(actualExecuteWatchdog.killedProcess());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ExecuteWatchdog#isWatching()}
   *   <li>{@link ExecuteWatchdog#killedProcess()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ExecuteWatchdog executeWatchdog = new ExecuteWatchdog(10);

    // Act
    boolean actualIsWatchingResult = executeWatchdog.isWatching();

    // Assert
    assertFalse(actualIsWatchingResult);
    assertFalse(executeWatchdog.killedProcess());
  }
}
