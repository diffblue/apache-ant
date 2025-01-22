package org.apache.tools.ant.types.selectors.modifiedselector;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class EqualComparatorDiffblueTest {
  /**
   * Test {@link EqualComparator#compare(Object, Object)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EqualComparator#compare(Object, Object)}
   */
  @Test
  public void testCompare_whenNull_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new EqualComparator()).compare(null, null));
  }

  /**
   * Test {@link EqualComparator#compare(Object, Object)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link EqualComparator#compare(Object, Object)}
   */
  @Test
  public void testCompare_whenNull_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new EqualComparator()).compare(null, "O2"));
  }

  /**
   * Test {@link EqualComparator#compare(Object, Object)}.
   * <ul>
   *   <li>When {@code O1}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EqualComparator#compare(Object, Object)}
   */
  @Test
  public void testCompare_whenO1_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new EqualComparator()).compare("O1", "O2"));
  }

  /**
   * Test {@link EqualComparator#compare(Object, Object)}.
   * <ul>
   *   <li>When {@code O2}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link EqualComparator#compare(Object, Object)}
   */
  @Test
  public void testCompare_whenO2_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new EqualComparator()).compare("O2", "O2"));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link EqualComparator}
   *   <li>{@link EqualComparator#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange, Act and Assert
    assertEquals("EqualComparator", (new EqualComparator()).toString());
  }
}
