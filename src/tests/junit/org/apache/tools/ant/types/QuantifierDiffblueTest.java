package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class QuantifierDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Quantifier#Quantifier()}
   *   <li>{@link Quantifier#getValues()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Quantifier actualQuantifier = new Quantifier();
    String[] actualValues = actualQuantifier.getValues();

    // Assert
    assertNull(actualQuantifier.getValue());
    assertEquals(-1, actualQuantifier.getIndex());
    assertArrayEquals(new String[]{"all", "each", "every", "any", "some", "one", "majority", "most", "none"},
        actualValues);
  }

  /**
   * Test {@link Quantifier#Quantifier(String)}.
   * <ul>
   *   <li>When {@code all}.</li>
   *   <li>Then return Value is {@code all}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#Quantifier(String)}
   */
  @Test
  public void testNewQuantifier_whenAll_thenReturnValueIsAll() {
    // Arrange and Act
    Quantifier actualQuantifier = new Quantifier("all");

    // Assert
    assertEquals("all", actualQuantifier.getValue());
    assertEquals(0, actualQuantifier.getIndex());
    assertArrayEquals(new String[]{"all", "each", "every", "any", "some", "one", "majority", "most", "none"},
        actualQuantifier.getValues());
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#ALL}.</li>
   *   <li>When array of {@code boolean} with {@code true} and {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenAll_whenArrayOfBooleanWithTrueAndFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.ALL.evaluate(new boolean[]{true, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#ALL}.</li>
   *   <li>When empty array of {@code boolean}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenAll_whenEmptyArrayOfBoolean_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.ALL.evaluate(new boolean[]{}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#ANY}.</li>
   *   <li>When array of {@code boolean} with {@code true} and {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenAny_whenArrayOfBooleanWithTrueAndFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.ANY.evaluate(new boolean[]{true, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#ANY}.</li>
   *   <li>When empty array of {@code boolean}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenAny_whenEmptyArrayOfBoolean_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.ANY.evaluate(new boolean[]{}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#MAJORITY}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenMajority_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.MAJORITY.evaluate(new boolean[]{true, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#MAJORITY}.</li>
   *   <li>When array of {@code boolean} with {@code true} and {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenMajority_whenArrayOfBooleanWithTrueAndTrue_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.MAJORITY.evaluate(new boolean[]{true, true, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#NONE}.</li>
   *   <li>When array of {@code boolean} with {@code true} and {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenNone_whenArrayOfBooleanWithTrueAndFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.NONE.evaluate(new boolean[]{true, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#NONE}.</li>
   *   <li>When empty array of {@code boolean}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenNone_whenEmptyArrayOfBoolean_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.NONE.evaluate(new boolean[]{}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#ONE}.</li>
   *   <li>When array of {@code boolean} with {@code false} and {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenOne_whenArrayOfBooleanWithFalseAndFalse_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.ONE.evaluate(new boolean[]{false, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#ONE}.</li>
   *   <li>When array of {@code boolean} with {@code true} and {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenOne_whenArrayOfBooleanWithTrueAndFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.ONE.evaluate(new boolean[]{true, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(boolean[])} with {@code b}.
   * <ul>
   *   <li>Given {@link Quantifier#Quantifier()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(boolean[])}
   */
  @Test
  public void testEvaluateWithB_givenQuantifier_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Quantifier()).evaluate(new boolean[]{true, false, true, false}));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#ALL}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenAll_whenOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.ALL.evaluate(1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#ALL}.</li>
   *   <li>When zero.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenAll_whenZero_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.ALL.evaluate(1, 0));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#ANY}.</li>
   *   <li>When minus one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenAny_whenMinusOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.ANY.evaluate(-1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#ANY}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenAny_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.ANY.evaluate(1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#MAJORITY}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenMajority_whenOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.MAJORITY.evaluate(1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#MAJORITY}.</li>
   *   <li>When six.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenMajority_whenSix_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.MAJORITY.evaluate(6, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#NONE}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenNone_whenOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.NONE.evaluate(1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#NONE}.</li>
   *   <li>When zero.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenNone_whenZero_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.NONE.evaluate(0, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#ONE}.</li>
   *   <li>When minus one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenOne_whenMinusOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Quantifier.ONE.evaluate(-1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#ONE}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenOne_whenOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Quantifier.ONE.evaluate(1, 1));
  }

  /**
   * Test {@link Quantifier#evaluate(int, int)} with {@code t}, {@code f}.
   * <ul>
   *   <li>Given {@link Quantifier#Quantifier()}.</li>
   *   <li>When one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Quantifier#evaluate(int, int)}
   */
  @Test
  public void testEvaluateWithTF_givenQuantifier_whenOne_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Quantifier()).evaluate(1, 1));
  }
}
